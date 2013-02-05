-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Supervisor -- {{{
    ( VisitorSupervisorActions(..)
    , VisitorSupervisorMonad
    , VisitorSupervisorResult(..)
    , VisitorSupervisorTerminationReason(..)
    , abortSupervisor
    , addWorker
    , disableSupervisorDebugMode
    , enableSupervisorDebugMode
    , getCurrentProgress
    , getNumberOfWorkers
    , getWaitingWorkers
    , performGlobalProgressUpdate
    , receiveProgressUpdate
    , receiveStolenWorkload
    , receiveWorkerFailure
    , receiveWorkerFinished
    , receiveWorkerFinishedAndRemoved
    , receiveWorkerFinishedWithRemovalFlag
    , removeWorker
    , removeWorkerIfPresent
    , runVisitorSupervisor
    , runVisitorSupervisorMaybeStartingFrom
    , runVisitorSupervisorStartingFrom
    , setSupervisorDebugMode
    ) where -- }}}

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative ((<$>),(<*>),Applicative)
import Control.Arrow (first,second)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),Exception(..),assert)
import Control.Monad (liftM2,mplus,unless,when)
import Control.Monad.CatchIO (MonadCatchIO,catch,throw)
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Control.Monad.Reader.Class as MonadsTF
import qualified Control.Monad.State.Class as MonadsTF
import Control.Monad.Reader (ask,asks)
import Control.Monad.Tools (ifM,whenM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Abort (AbortT(..),abort,runAbortT,unwrapAbortT)
import Control.Monad.Trans.Abort.Instances.MonadsTF
import Control.Monad.Trans.Reader (ReaderT,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalStateT,runStateT)

import Data.Accessor.Monad.TF.State ((%=),(%:),get,getAndModify)
import Data.Accessor.Template (deriveAccessors)
import Data.Composition ((.*))
import Data.Either.Unwrap (whenLeft)
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (inits)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust,fromMaybe,isJust,isNothing)
import Data.Monoid (Monoid(..))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)

import qualified System.Log.Logger as Logger

import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Path
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Exceptions {{{

-- InconsistencyError {{{
data InconsistencyError = -- {{{
    IncompleteWorkspace VisitorCheckpoint
  | OutOfSourcesForNewWorkloads
  | SpaceFullyExploredButSearchNotTerminated
  deriving (Eq,Typeable)
-- }}}
instance Show InconsistencyError where -- {{{
    show (IncompleteWorkspace workspace) = "Full workspace is incomplete: " ++ show workspace
    show OutOfSourcesForNewWorkloads = "The search is incomplete but there are no sources of workloads available."
    show SpaceFullyExploredButSearchNotTerminated = "The space has been fully explored but the search was not terminated."
-- }}}
instance Exception InconsistencyError
-- }}}

-- WorkerManagementError {{{
data WorkerManagementError worker_id = -- {{{
    ActiveWorkersRemainedAfterSpaceFullyExplored [worker_id]
  | ConflictingWorkloads (Maybe worker_id) VisitorPath (Maybe worker_id) VisitorPath
  | SpaceFullyExploredButWorkloadsRemain [(Maybe worker_id,VisitorWorkload)]
  | WorkerAlreadyKnown String worker_id
  | WorkerAlreadyHasWorkload worker_id
  | WorkerNotKnown String worker_id
  | WorkerNotActive String worker_id
  deriving (Eq,Typeable)
 -- }}}
instance Show worker_id ⇒ Show (WorkerManagementError worker_id) where -- {{{
    show (ActiveWorkersRemainedAfterSpaceFullyExplored worker_ids) = "Worker ids " ++ show worker_ids ++ " were still active after the space was supposedly fully explored."
    show (ConflictingWorkloads wid1 path1 wid2 path2) =
        "Workload " ++ f wid1 ++ " with initial path " ++ show path1 ++ " conflicts with workload " ++ f wid2 ++ " with initial path " ++ show path2 ++ "."
      where
        f Nothing = "sitting idle"
        f (Just wid) = "of worker " ++ show wid
    show (SpaceFullyExploredButWorkloadsRemain workers_and_workloads) = "The space has been fully explored, but the following workloads remain: " ++ show workers_and_workloads
    show (WorkerAlreadyKnown action worker_id) = "Worker id " ++ show worker_id ++ " already known when " ++ action ++ "."
    show (WorkerAlreadyHasWorkload worker_id) = "Attempted to send workload to worker id " ++ show worker_id ++ " which is already active."
    show (WorkerNotKnown action worker_id) = "Worker id " ++ show worker_id ++ " not known when " ++ action ++ "."
    show (WorkerNotActive action worker_id) = "Worker id " ++ show worker_id ++ " not active when " ++ action ++ "."
-- }}}
instance (Eq worker_id, Show worker_id, Typeable worker_id) ⇒ Exception (WorkerManagementError worker_id)
-- }}}

-- SupervisorError {{{
data SupervisorError worker_id = -- {{{
    SupervisorInconsistencyError InconsistencyError
  | SupervisorWorkerManagementError (WorkerManagementError worker_id)
  deriving (Eq,Typeable)
-- }}}
instance Show worker_id ⇒ Show (SupervisorError worker_id) where -- {{{
    show (SupervisorInconsistencyError e) = show e
    show (SupervisorWorkerManagementError e) = show e
-- }}}
instance (Eq worker_id, Show worker_id, Typeable worker_id) ⇒ Exception (SupervisorError worker_id) where -- {{{
    toException (SupervisorInconsistencyError e) = toException e
    toException (SupervisorWorkerManagementError e) = toException e
    fromException e =
        (SupervisorInconsistencyError <$> fromException e)
        `mplus`
        (SupervisorWorkerManagementError <$> fromException e)
-- }}}
-- }}}

-- }}}

-- Types {{{

data VisitorSupervisorActions result worker_id m = -- {{{
    VisitorSupervisorActions
    {   broadcast_progress_update_to_workers_action :: [worker_id] → m ()
    ,   broadcast_workload_steal_to_workers_action :: [worker_id] → m ()
    ,   receive_current_progress_action :: VisitorProgress result → m ()
    ,   send_workload_to_worker_action :: VisitorWorkload → worker_id → m ()
    }
-- }}}

data VisitorSupervisorState result worker_id = -- {{{
    VisitorSupervisorState
    {   waiting_workers_or_available_workloads_ :: !(Either (Set worker_id) (Set VisitorWorkload))
    ,   known_workers_ :: !(Set worker_id)
    ,   active_workers_ :: !(Map worker_id VisitorWorkload)
    ,   current_steal_depth_ :: !Int
    ,   available_workers_for_steal_ :: !(IntMap (Set worker_id))
    ,   workers_pending_workload_steal_ :: !(Set worker_id)
    ,   workers_pending_progress_update_ :: !(Set worker_id)
    ,   current_progress_ :: !(VisitorProgress result)
    ,   debug_mode_ :: !Bool
    }
$( deriveAccessors ''VisitorSupervisorState )
-- }}}

data VisitorSupervisorTerminationReason result worker_id =
    SupervisorAborted (VisitorProgress result)
  | SupervisorCompleted result
  | SupervisorFailure worker_id String
  deriving (Eq,Show)

data VisitorSupervisorResult result worker_id =
    VisitorSupervisorResult
    {   visitorSupervisorTerminationReason :: VisitorSupervisorTerminationReason result worker_id
    ,   visitorSupervisorRemainingWorkers :: [worker_id]
    } deriving (Eq,Show)

type VisitorSupervisorContext result worker_id m = -- {{{
    StateT (VisitorSupervisorState result worker_id)
        (ReaderT (VisitorSupervisorActions result worker_id m) m)
-- }}}

type VisitorSupervisorAbortMonad result worker_id m =
    AbortT
        (VisitorSupervisorResult result worker_id)
        (VisitorSupervisorContext result worker_id m)

newtype VisitorSupervisorMonad result worker_id m α = -- {{{
    VisitorSupervisorMonad {
        unwrapVisitorSupervisorMonad :: VisitorSupervisorAbortMonad result worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

-- }}}

-- Instances {{{

instance MonadTrans (VisitorSupervisorMonad result worker_id) where -- {{{
    lift = VisitorSupervisorMonad . lift . liftUserToContext
-- }}}

instance MonadsTF.MonadReader m ⇒ MonadsTF.MonadReader (VisitorSupervisorMonad result worker_id m) where -- {{{
    type EnvType (VisitorSupervisorMonad result worker_id m) = MonadsTF.EnvType m
    ask = lift MonadsTF.ask
    local f m = VisitorSupervisorMonad $ do
        actions ← MonadsTF.ask
        old_state ← MonadsTF.get
        (result,new_state) ←
            lift
            .
            lift
            .
            lift
            .
            MonadsTF.local f
            .
            flip runReaderT actions
            .
            flip runStateT old_state
            .
            unwrapAbortT
            .
            unwrapVisitorSupervisorMonad
            $
            m
        MonadsTF.put new_state
        either abort return result
-- }}}

instance MonadsTF.MonadState m ⇒ MonadsTF.MonadState (VisitorSupervisorMonad result worker_id m) where -- {{{
    type StateType (VisitorSupervisorMonad result worker_id m) = MonadsTF.StateType m
    get = lift MonadsTF.get
    put = lift . MonadsTF.put
-- }}}

-- }}}

-- Exposed functions {{{

abortSupervisor :: (Functor m, Monad m) ⇒ VisitorSupervisorMonad result worker_id m α -- {{{
abortSupervisor = VisitorSupervisorMonad $
    get current_progress >>= abortSupervisorWithReason . SupervisorAborted
-- }}}

addWorker :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
addWorker worker_id = postValidate ("addWorker " ++ show worker_id) . VisitorSupervisorMonad . lift $ do
    infoM $ "Adding worker " ++ show worker_id
    validateWorkerNotKnown "adding worker" worker_id
    known_workers %: Set.insert worker_id
    tryToObtainWorkloadFor worker_id
-- }}}

disableSupervisorDebugMode :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m ()
disableSupervisorDebugMode = setSupervisorDebugMode False
-- }}}

enableSupervisorDebugMode :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m ()
enableSupervisorDebugMode = setSupervisorDebugMode True
-- }}}

getCurrentProgress :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m (VisitorProgress result)
getCurrentProgress = VisitorSupervisorMonad . lift . get $ current_progress
-- }}}

getNumberOfWorkers :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m Int
getNumberOfWorkers = VisitorSupervisorMonad . lift . (Set.size <$>) . get $ known_workers
-- }}}

getWaitingWorkers :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m (Set worker_id)
getWaitingWorkers = VisitorSupervisorMonad . lift $
    either id (const Set.empty) <$> get waiting_workers_or_available_workloads
-- }}}

performGlobalProgressUpdate :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m ()
performGlobalProgressUpdate = postValidate "performGlobalProgressUpdate" . VisitorSupervisorMonad . lift $ do
    infoM $ "Performing global progress update."
    active_worker_ids ← Map.keysSet <$> get active_workers
    if (Set.null active_worker_ids)
        then receiveCurrentProgress
        else do
            workers_pending_progress_update %= active_worker_ids
            asks broadcast_progress_update_to_workers_action >>= liftUserToContext . ($ Set.toList active_worker_ids)
-- }}}

receiveProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorWorkerProgressUpdate result →
    VisitorSupervisorMonad result worker_id m ()
receiveProgressUpdate worker_id (VisitorWorkerProgressUpdate progress_update remaining_workload) = postValidate ("receiveProgressUpdate " ++ show worker_id ++ " ...") . VisitorSupervisorMonad . lift $ do
    infoM $ "Received progress update from " ++ show worker_id
    validateWorkerKnownAndActive "receiving progress update" worker_id
    current_progress %: (`mappend` progress_update)
    is_pending_workload_steal ← Set.member worker_id <$> get workers_pending_workload_steal
    unless is_pending_workload_steal $ dequeueWorkerForSteal worker_id
    active_workers %: Map.insert worker_id remaining_workload
    unless is_pending_workload_steal $ enqueueWorkerForSteal worker_id
    clearPendingProgressUpdate worker_id
-- }}}

receiveStolenWorkload :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    Maybe (VisitorWorkerStolenWorkload result) →
    VisitorSupervisorMonad result worker_id m ()
receiveStolenWorkload worker_id maybe_stolen_workload = postValidate ("receiveStolenWorkload " ++ show worker_id ++ " ...") . VisitorSupervisorMonad . lift $ do
    infoM $ "Received stolen workload from " ++ show worker_id
    validateWorkerKnownAndActive "receiving stolen workload" worker_id
    workers_pending_workload_steal %: Set.delete worker_id
    case maybe_stolen_workload of
        Nothing → return ()
        Just (VisitorWorkerStolenWorkload (VisitorWorkerProgressUpdate progress_update remaining_workload) workload) → do
            current_progress %: (`mappend` progress_update)
            active_workers %: Map.insert worker_id remaining_workload
            enqueueWorkload workload
    enqueueWorkerForSteal worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

receiveWorkerFailure :: (Functor m, Monad m) ⇒ worker_id → String → VisitorSupervisorMonad result worker_id m α -- {{{
receiveWorkerFailure =
    (VisitorSupervisorMonad . abortSupervisorWithReason)
    .*
    SupervisorFailure
-- }}}

receiveWorkerFinished :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorProgress result →
    VisitorSupervisorMonad result worker_id m ()
receiveWorkerFinished = receiveWorkerFinishedWithRemovalFlag False
-- }}}

receiveWorkerFinishedAndRemoved :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorProgress result →
    VisitorSupervisorMonad result worker_id m ()
receiveWorkerFinishedAndRemoved = receiveWorkerFinishedWithRemovalFlag True
-- }}}

receiveWorkerFinishedWithRemovalFlag :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    Bool →
    worker_id →
    VisitorProgress result →
    VisitorSupervisorMonad result worker_id m ()
receiveWorkerFinishedWithRemovalFlag remove_worker worker_id final_progress = postValidate ("receiveWorkerFinished " ++ show worker_id ++ " " ++ show (visitorCheckpoint final_progress)) . VisitorSupervisorMonad $ do
    infoM $ if remove_worker
        then "Worker " ++ show worker_id ++ " finished and removed."
        else "Worker " ++ show worker_id ++ " finished."
    lift $ validateWorkerKnownAndActive "the worker was declared finished" worker_id
    current_progress %: (`mappend` final_progress)
    when remove_worker $ known_workers %: Set.delete worker_id
    VisitorProgress checkpoint new_results ← get current_progress
    case checkpoint of
        Explored → do
            active_worker_ids ← Map.keys . Map.delete worker_id <$> get active_workers
            unless (null active_worker_ids) . throw $
                ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
            known_worker_ids ← Set.toList <$> get known_workers
            abort $ VisitorSupervisorResult (SupervisorCompleted new_results) known_worker_ids
        _ → lift $ do
            deactivateWorker False worker_id
            unless remove_worker $ tryToObtainWorkloadFor worker_id
-- }}}

removeWorker :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
removeWorker worker_id = postValidate ("removeWorker " ++ show worker_id) . VisitorSupervisorMonad . lift $ do
    infoM $ "Removing worker " ++ show worker_id
    validateWorkerKnown "removing the worker" worker_id
    known_workers %: Set.delete worker_id
    ifM (isJust . Map.lookup worker_id <$> get active_workers)
        (deactivateWorker True worker_id)
        (waiting_workers_or_available_workloads %: either (Left . Set.delete worker_id) Right)
-- }}}

removeWorkerIfPresent :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
removeWorkerIfPresent worker_id = postValidate ("removeWorker " ++ show worker_id) . VisitorSupervisorMonad . lift $ do
    whenM (Set.member worker_id <$> get known_workers) $ do
        infoM $ "Removing worker " ++ show worker_id
        known_workers %: Set.delete worker_id
        ifM (isJust . Map.lookup worker_id <$> get active_workers)
            (deactivateWorker True worker_id)
            (waiting_workers_or_available_workloads %: either (Left . Set.delete worker_id) Right)
-- }}}

runVisitorSupervisor :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorActions result worker_id m →
    (∀ a. VisitorSupervisorMonad result worker_id m a) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisor = runVisitorSupervisorStartingFrom mempty
-- }}}

runVisitorSupervisorMaybeStartingFrom :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    Maybe (VisitorProgress result) →
    VisitorSupervisorActions result worker_id m →
    (∀ a. VisitorSupervisorMonad result worker_id m a) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisorMaybeStartingFrom Nothing = runVisitorSupervisor
runVisitorSupervisorMaybeStartingFrom (Just progress) = runVisitorSupervisorStartingFrom progress
-- }}}

runVisitorSupervisorStartingFrom :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorProgress result →
    VisitorSupervisorActions result worker_id m →
    (∀ α. VisitorSupervisorMonad result worker_id m α) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisorStartingFrom starting_progress actions loop =
    flip runReaderT actions
    .
    flip evalStateT
        (VisitorSupervisorState
            {   waiting_workers_or_available_workloads_ =
                    Right . Set.singleton $ VisitorWorkload Seq.empty (visitorCheckpoint starting_progress)
            ,   known_workers_ = mempty
            ,   active_workers_ = mempty
            ,   current_steal_depth_ = 0
            ,   available_workers_for_steal_ = mempty
            ,   workers_pending_workload_steal_ = mempty
            ,   workers_pending_progress_update_ = mempty
            ,   current_progress_ = starting_progress
            ,   debug_mode_ = False
            }
        )
    .
    runAbortT
    .
    (
        AbortT
        .
        (flip catch $ \e →
            let abortIt = unwrapAbortT . unwrapVisitorSupervisorMonad $ abortSupervisor
            in case fromException e of
                Just ThreadKilled → abortIt
                Just UserInterrupt → abortIt
                _ → throw e
        )
        .
        unwrapAbortT
    )
    .
    unwrapVisitorSupervisorMonad
    $
    loop
-- }}}

setSupervisorDebugMode :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    Bool →
    VisitorSupervisorMonad result worker_id m ()
setSupervisorDebugMode = VisitorSupervisorMonad . lift . (debug_mode %=)
-- }}}

-- }}}

-- Logging Functions {{{
debugM :: MonadIO m ⇒ String → m ()
debugM = liftIO . Logger.debugM "Supervisor"

infoM :: MonadIO m ⇒ String → m ()
infoM = liftIO . Logger.infoM "Supervisor"
-- }}}

-- Internal Functions {{{

abortSupervisorWithReason ::  -- {{{
    (Functor m, Monad m) ⇒
    VisitorSupervisorTerminationReason result worker_id →
    VisitorSupervisorAbortMonad result worker_id m α
abortSupervisorWithReason reason =
    (VisitorSupervisorResult
        <$> (return reason)
        <*> (Set.toList <$> get known_workers)
    ) >>= abort
-- }}}

checkWhetherMoreStealsAreNeeded :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m ()
checkWhetherMoreStealsAreNeeded = do
    number_of_waiting_workers ← either Set.size (const 0) <$> get waiting_workers_or_available_workloads
    number_of_pending_workload_steals ← Set.size <$> get workers_pending_workload_steal
    available_workers ← get available_workers_for_steal
    when (number_of_pending_workload_steals == 0
       && number_of_waiting_workers > 0
       && IntMap.null available_workers
      ) $ throw OutOfSourcesForNewWorkloads
    let number_of_needed_steals = (number_of_waiting_workers - number_of_pending_workload_steals) `max` 0
    when (number_of_needed_steals > 0 && (not . IntMap.null) available_workers) $ do
        depth ← do
            old_depth ← get current_steal_depth
            if number_of_pending_workload_steals == 0 && IntMap.notMember old_depth available_workers
                then do
                    let new_depth = fst (IntMap.findMin available_workers)
                    current_steal_depth %= new_depth
                    return new_depth
                else return old_depth
        let (maybe_new_workers,workers_to_steal_from) =
                go []
                   (fromMaybe Set.empty . IntMap.lookup depth $ available_workers)
                   number_of_needed_steals
              where
                go accum (Set.minView → Nothing) _ = (Nothing,accum)
                go accum workers 0 = (Just workers,accum)
                go accum (Set.minView → Just (worker_id,rest_workers)) n =
                    go (worker_id:accum) rest_workers (n-1)
        workers_pending_workload_steal %: (Set.union . Set.fromList) workers_to_steal_from
        available_workers_for_steal %: IntMap.update (const maybe_new_workers) depth
        unless (null workers_to_steal_from) $ do
            infoM $ "Sending workload steal requests to " ++ show workers_to_steal_from
            asks broadcast_workload_steal_to_workers_action >>= liftUserToContext . ($ workers_to_steal_from)
-- }}}

clearPendingProgressUpdate :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
clearPendingProgressUpdate worker_id =
    Set.member worker_id <$> get workers_pending_progress_update >>= flip when
    -- Note, the conditional above is needed to prevent a "misfire" where
    -- we think that we have just completed a progress update even though
    -- none was started.
    (do workers_pending_progress_update %: Set.delete worker_id
        no_progress_updates_are_pending ← Set.null <$> get workers_pending_progress_update
        when no_progress_updates_are_pending receiveCurrentProgress
    )
-- }}}

deactivateWorker :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    Bool →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
deactivateWorker reenqueue_workload worker_id = do
    workers_pending_workload_steal %: Set.delete worker_id
    dequeueWorkerForSteal worker_id
    if reenqueue_workload
        then getAndModify active_workers (Map.delete worker_id)
              >>=
                enqueueWorkload
                .
                fromMaybe (error $ "Attempt to deactive worker " ++ show worker_id ++ " which was not listed as active.")
                .
                Map.lookup worker_id
        else active_workers %: Map.delete worker_id
    clearPendingProgressUpdate worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

dequeueWorkerForSteal :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
dequeueWorkerForSteal worker_id =
    getWorkerDepth worker_id >>= \depth →
        available_workers_for_steal %:
            IntMap.update
                (\queue → let new_queue = Set.delete worker_id queue
                          in if Set.null new_queue then Nothing else Just new_queue
                )
                depth
-- }}}

enqueueWorkerForSteal :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
enqueueWorkerForSteal worker_id =
    getWorkerDepth worker_id >>= \depth →
        available_workers_for_steal %:
            IntMap.alter
                (Just . maybe (Set.singleton worker_id) (Set.insert worker_id))
                depth
-- }}}

enqueueWorkload :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    VisitorSupervisorContext result worker_id m ()
enqueueWorkload workload =
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left (Set.minView → Just (free_worker_id,remaining_workers)) → do
            waiting_workers_or_available_workloads %= Left remaining_workers
            sendWorkloadToWorker workload free_worker_id
        Left (Set.minView → Nothing) →
            waiting_workers_or_available_workloads %= Right (Set.singleton workload)
        Right available_workloads →
            waiting_workers_or_available_workloads %= Right (Set.insert workload available_workloads)
-- }}}

getWorkerDepth :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m Int
getWorkerDepth worker_id =
    maybe
        (error $ "Attempted to get the depth of inactive worker " ++ show worker_id ++ ".")
        workloadDepth
    .
    Map.lookup worker_id
    <$>
    get active_workers
-- }}}

liftUserToContext :: Monad m ⇒ m α → VisitorSupervisorContext result worker_id m α -- {{{
liftUserToContext = lift . lift
-- }}}

postValidate :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    VisitorSupervisorMonad result worker_id m α →
    VisitorSupervisorMonad result worker_id m α
postValidate label action = action >>= \result → VisitorSupervisorMonad . lift $
  (whenDebugging $ do
    debugM $ " === BEGIN VALIDATE === " ++ label
    get known_workers >>= debugM . ("Known workers is now " ++) . show
    get active_workers >>= debugM . ("Active workers is now " ++) . show
    get waiting_workers_or_available_workloads >>= debugM . ("Waiting/Available queue is now " ++) . show
    get current_progress >>= debugM . ("Current checkpoint is now " ++) . show . visitorCheckpoint
    workers_and_workloads ←
        liftM2 (++)
            (map (Nothing,) . Set.toList . either (const (Set.empty)) id <$> get waiting_workers_or_available_workloads)
            (map (first Just) . Map.assocs <$> get active_workers)
    let go [] _ = return ()
        go ((maybe_worker_id,VisitorWorkload initial_path _):rest_workloads) known_prefixes =
            case Map.lookup initial_path_as_list known_prefixes of
                Nothing → go rest_workloads . mappend known_prefixes . Map.fromList . map (,(maybe_worker_id,initial_path)) . inits $ initial_path_as_list
                Just (maybe_other_worker_id,other_initial_path) →
                    throw $ ConflictingWorkloads maybe_worker_id initial_path maybe_other_worker_id other_initial_path
          where initial_path_as_list = Fold.toList initial_path
    go workers_and_workloads Map.empty
    VisitorProgress checkpoint _ ← get current_progress
    let total_workspace =
            mappend checkpoint
            .
            mconcat
            .
            map (flip checkpointFromInitialPath Explored . visitorWorkloadPath . snd)
            $
            workers_and_workloads
    unless (total_workspace == Explored) $ throw $ IncompleteWorkspace total_workspace
    VisitorProgress checkpoint _ ← get current_progress
    when (checkpoint == Explored) $
        if null workers_and_workloads
            then throw $ SpaceFullyExploredButSearchNotTerminated
            else throw $ SpaceFullyExploredButWorkloadsRemain workers_and_workloads
    debugM $ " === END VALIDATE === " ++ label
  ) >> return result
-- }}}

receiveCurrentProgress :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m ()
receiveCurrentProgress = do
    callback ← asks receive_current_progress_action
    current_progress ← get current_progress
    liftUserToContext (callback current_progress)
-- }}}

sendWorkloadToWorker :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
sendWorkloadToWorker workload worker_id = do
    infoM $ "Sending workload to " ++ show worker_id
    asks send_workload_to_worker_action >>= liftUserToContext . (\f → f workload worker_id)
    isNothing . Map.lookup worker_id <$> get active_workers
        >>= flip unless (throw $ WorkerAlreadyHasWorkload worker_id)
    active_workers %: Map.insert worker_id workload
    enqueueWorkerForSteal worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

tryToObtainWorkloadFor :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
tryToObtainWorkloadFor worker_id =
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers → do
            waiting_workers_or_available_workloads %= Left (Set.insert worker_id waiting_workers)
            checkWhetherMoreStealsAreNeeded
        Right (Set.minView → Nothing) → do
            waiting_workers_or_available_workloads %= Left (Set.singleton worker_id)
            checkWhetherMoreStealsAreNeeded
        Right (Set.minView → Just (workload,remaining_workloads)) → do
            sendWorkloadToWorker workload worker_id
            waiting_workers_or_available_workloads %= Right remaining_workloads
-- }}}

validateWorkerKnown :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerKnown action worker_id =
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotKnown action worker_id)
-- }}}

validateWorkerKnownAndActive :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerKnownAndActive action worker_id = do
    validateWorkerKnown action worker_id
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotActive action worker_id)
-- }}}

validateWorkerNotKnown :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerNotKnown action worker_id = do
    Set.member worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown action worker_id)
-- }}}

whenDebugging :: -- {{{
    (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m () →
    VisitorSupervisorContext result worker_id m ()
whenDebugging action = get debug_mode >>= flip when action
-- }}}

-- }}}
