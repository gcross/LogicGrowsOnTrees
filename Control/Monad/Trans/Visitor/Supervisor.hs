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
    , abortSupervisor
    , addWorker
    , getCurrentProgress
    , getNumberOfWorkers
    , getWaitingWorkers
    , performGlobalProgressUpdate
    , receiveProgressUpdate
    , receiveStolenWorkload
    , receiveWorkerFinished
    , receiveWorkerFinishedAndRemoved
    , receiveWorkerFinishedWithRemovalFlag
    , removeWorker
    , runVisitorSupervisor
    , runVisitorSupervisorStartingFrom
    ) where -- }}}

-- Imports {{{
import Control.Applicative ((<$>),(<*>),Applicative)
import Control.Arrow (first,second)
import Control.Exception (Exception(..),assert)
import Control.Monad (liftM2,mplus,unless,when)
import Control.Monad.CatchIO (MonadCatchIO,throw)
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Control.Monad.State.Class as MonadsTF
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Abort (AbortT,abort,runAbortT)
import Control.Monad.Trans.RWS.Strict (RWST,asks,evalRWST)

import Data.Accessor.Monad.TF.State ((%=),(%:),get)
import Data.Accessor.Template (deriveAccessors)
import Data.Either.Unwrap (whenLeft)
import qualified Data.Foldable as Fold
import Data.List (inits)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust,isNothing)
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
    {   waiting_workers_or_available_workloads_ :: !(Either (Set worker_id) [VisitorWorkload])
    ,   known_workers_ :: !(Set worker_id)
    ,   active_workers_ :: !(Map worker_id VisitorWorkload)
    ,   workers_pending_workload_steal_ :: !(Set worker_id)
    ,   workers_pending_progress_update_ :: !(Set worker_id)
    ,   current_progress_ :: !(VisitorProgress result)
    }
$( deriveAccessors ''VisitorSupervisorState )
-- }}}

data VisitorSupervisorResult result worker_id = VisitorSupervisorResult (Either (VisitorProgress result) result) [worker_id] deriving (Eq,Show)

type VisitorSupervisorContext result worker_id m = -- {{{
    RWST
        (VisitorSupervisorActions result worker_id m)
        ()
        (VisitorSupervisorState result worker_id)
        m
-- }}}

newtype VisitorSupervisorMonad result worker_id m a = -- {{{
    VisitorSupervisorMonad {
      unwrapVisitorSupervisorMonad ::
        (AbortT
            (VisitorSupervisorResult result worker_id)
            (VisitorSupervisorContext result worker_id m)
            a
        )
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

-- }}}

-- Instances {{{

instance MonadTrans (VisitorSupervisorMonad result worker_id) where -- {{{
    lift = VisitorSupervisorMonad . lift . lift
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
    (lift
     $
     VisitorSupervisorResult
        <$> (Left <$> get current_progress)
        <*> (Set.toList <$> get known_workers)
    )
    >>= abort
-- }}}

addWorker :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
addWorker worker_id = postValidate ("addWorker " ++ show worker_id) . VisitorSupervisorMonad . lift $ do
    validateWorkerNotKnown "adding worker" worker_id
    known_workers %: Set.insert worker_id
    tryToObtainWorkloadFor worker_id
-- }}}

getCurrentProgress :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m (VisitorProgress result)
getCurrentProgress = VisitorSupervisorMonad . lift . get $ current_progress
-- }}}

getNumberOfWorkers :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m Int
getNumberOfWorkers = VisitorSupervisorMonad . lift . (Set.size <$>) . get $ known_workers
-- }}}

getWaitingWorkers :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m (Set worker_id)
getWaitingWorkers = VisitorSupervisorMonad . lift $
    either id (const Set.empty) <$> get waiting_workers_or_available_workloads
-- }}}

performGlobalProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m ()
performGlobalProgressUpdate = postValidate "performGlobalProgressUpdate" . VisitorSupervisorMonad . lift $ do
    active_worker_ids ← Map.keysSet <$> get active_workers
    if (Set.null active_worker_ids)
        then receiveCurrentProgress
        else do
            workers_pending_progress_update %= active_worker_ids
            asks broadcast_progress_update_to_workers_action >>= lift . ($ Set.toList active_worker_ids)
-- }}}

receiveProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorWorkerProgressUpdate result →
    VisitorSupervisorMonad result worker_id m ()
receiveProgressUpdate worker_id (VisitorWorkerProgressUpdate progress_update remaining_workload) = postValidate ("receiveProgressUpdate " ++ show worker_id ++ " ...") . VisitorSupervisorMonad . lift $ do
    validateWorkerKnownAndActive "receiving progress update" worker_id
    current_progress %: (`mappend` progress_update)
    active_workers %: (Map.insert worker_id remaining_workload)
    clearPendingProgressUpdate worker_id
-- }}}

receiveStolenWorkload :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    Maybe (VisitorWorkerStolenWorkload result) →
    VisitorSupervisorMonad result worker_id m ()
receiveStolenWorkload worker_id maybe_stolen_workload = postValidate ("receiveStolenWorkload " ++ show worker_id ++ " ...") . VisitorSupervisorMonad . lift $ do
    validateWorkerKnownAndActive "receiving stolen workload" worker_id
    case maybe_stolen_workload of
        Nothing → return ()
        Just (VisitorWorkerStolenWorkload (VisitorWorkerProgressUpdate progress_update remaining_workload) workload) → do
            current_progress %: (`mappend` progress_update)
            active_workers %: (Map.insert worker_id remaining_workload)
            enqueueWorkload workload
    clearPendingWorkloadSteal worker_id
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
receiveWorkerFinishedWithRemovalFlag remove_worker worker_id final_progress = postValidate ("receiveWorkerFinished " ++ show worker_id ++ " " ++ show (visitorCheckpoint final_progress)) . VisitorSupervisorMonad $
    (lift $ do
        validateWorkerKnownAndActive "the worker was declared finished" worker_id
        clearPendingResponses worker_id
        active_workers %: Map.delete worker_id
        current_progress %: (`mappend` final_progress)
        when remove_worker $ known_workers %: Set.delete worker_id
        VisitorProgress checkpoint new_results ← get current_progress
        case checkpoint of
            Explored → do
                active_worker_ids ← Map.keys <$> get active_workers
                unless (null active_worker_ids) $
                    throw $ ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
                known_worker_ids ← Set.toList <$> get known_workers
                return . Just $ VisitorSupervisorResult (Right new_results) known_worker_ids
            _ → do
                unless remove_worker $ tryToObtainWorkloadFor worker_id
                return Nothing
    ) >>= maybe (return ()) abort
-- }}}

removeWorker :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
removeWorker worker_id = postValidate ("removeWorker " ++ show worker_id) . VisitorSupervisorMonad . lift $ do
    validateWorkerKnown "removing the worker" worker_id
    known_workers %: Set.delete worker_id
    Map.lookup worker_id <$> get active_workers >>= \maybe_workload →
        case maybe_workload of
            Just workload → do
                active_workers %: Map.delete worker_id
                enqueueWorkload workload
                clearPendingResponses worker_id
            Nothing → do
                waiting_workers_or_available_workloads %: either (Left . Set.delete worker_id) Right
-- }}}

runVisitorSupervisor :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorActions result worker_id m →
    (∀ a. VisitorSupervisorMonad result worker_id m a) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisor = runVisitorSupervisorStartingFrom mempty
-- }}}

runVisitorSupervisorStartingFrom :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorProgress result →
    VisitorSupervisorActions result worker_id m →
    (∀ a. VisitorSupervisorMonad result worker_id m a) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisorStartingFrom starting_progress actions loop =
    (fst <$>)
    .
    (\x ->
     evalRWST
        x
        actions
        (VisitorSupervisorState
            {   waiting_workers_or_available_workloads_ =
                    Right [VisitorWorkload Seq.empty (visitorCheckpoint starting_progress)  ]
            ,   known_workers_ = mempty
            ,   active_workers_ = mempty
            ,   workers_pending_workload_steal_ = mempty
            ,   workers_pending_progress_update_ = mempty
            ,   current_progress_ = starting_progress
            }
        )
    )
    .
    runAbortT
    .
    unwrapVisitorSupervisorMonad
    $
    loop'
  where
    loop' = loop
-- }}}

-- }}}

-- Logging Functions {{{
debugM :: MonadIO m ⇒ String → m ()
debugM = liftIO . Logger.debugM "Supervisor"
-- }}}

-- Internal Functions {{{

broadcastWorkloadStealToActiveWorkers :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m ()
broadcastWorkloadStealToActiveWorkers = do
    active_worker_ids ← Map.keysSet <$> get active_workers
    when (Set.null active_worker_ids) $ throw OutOfSourcesForNewWorkloads
    asks broadcast_workload_steal_to_workers_action >>= lift . ($ Set.toList active_worker_ids)
    workers_pending_workload_steal %= active_worker_ids
-- }}}

clearPendingProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
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

clearPendingResponses :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
clearPendingResponses worker_id = do
    clearPendingProgressUpdate worker_id
    clearPendingWorkloadSteal worker_id
-- }}}

clearPendingWorkloadSteal :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
clearPendingWorkloadSteal worker_id =
    Set.member worker_id <$> get workers_pending_workload_steal >>= flip when
    -- Note, the conditional above is needed to prevent a "misfire" where
    -- we think that we have just completed a workload steal even though
    -- none was started.
    (do workers_pending_workload_steal %: Set.delete worker_id
        no_workload_steals_remain ← Set.null <$> get workers_pending_workload_steal
        workers_are_waiting_for_workloads ← either (not . Set.null) (const False) <$> get waiting_workers_or_available_workloads
        when (no_workload_steals_remain && workers_are_waiting_for_workloads) broadcastWorkloadStealToActiveWorkers
    )
-- }}}

enqueueWorkload :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    VisitorSupervisorContext result worker_id m ()
enqueueWorkload workload =
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left (Set.minView → Just (free_worker_id,remaining_workers)) → do
            sendWorkloadToWorker workload free_worker_id
            waiting_workers_or_available_workloads %= Left remaining_workers
        Left (Set.minView → Nothing) →
            waiting_workers_or_available_workloads %= Right [workload]
        Right available_workloads →
            waiting_workers_or_available_workloads %= Right (workload:available_workloads)
-- }}}

postValidate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    VisitorSupervisorMonad result worker_id m α →
    VisitorSupervisorMonad result worker_id m α
postValidate label action = action >>= \result → VisitorSupervisorMonad . lift $ do
    debugM $ " === BEGIN VALIDATE === " ++ label
    get known_workers >>= debugM . ("Known workers is now " ++) . show
    get active_workers >>= debugM . ("Active workers is now " ++) . show
    get waiting_workers_or_available_workloads >>= debugM . ("Waiting/Available queue is now " ++) . show
    get current_progress >>= debugM . ("Current checkpoint is now " ++) . show . visitorCheckpoint
    workers_and_workloads ←
        liftM2 (++)
            (map (Nothing,) . either (const []) id <$> get waiting_workers_or_available_workloads)
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
    return result
-- }}}

receiveCurrentProgress :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m ()
receiveCurrentProgress = do
    callback ← asks receive_current_progress_action
    current_progress ← get current_progress
    lift (callback current_progress)
-- }}}

sendWorkloadToWorker :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
sendWorkloadToWorker workload worker_id = do
    asks send_workload_to_worker_action >>= lift . (\f → f workload worker_id)
    isNothing . Map.lookup worker_id <$> get active_workers
        >>= flip unless (throw $ WorkerAlreadyHasWorkload worker_id)
    active_workers %: Map.insert worker_id workload
-- }}}

tryToObtainWorkloadFor :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
tryToObtainWorkloadFor worker_id =
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers →
            waiting_workers_or_available_workloads %= Left (Set.insert worker_id waiting_workers)
        Right [] → do
            broadcastWorkloadStealToActiveWorkers
            waiting_workers_or_available_workloads %= Left (Set.singleton worker_id)
        Right (workload:remaining_workloads) → do
            sendWorkloadToWorker workload worker_id
            waiting_workers_or_available_workloads %= Right remaining_workloads
-- }}}

validateWorkerKnown :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerKnown action worker_id =
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotKnown action worker_id)
-- }}}

validateWorkerKnownAndActive :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerKnownAndActive action worker_id = do
    validateWorkerKnown action worker_id
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotActive action worker_id)
-- }}}

validateWorkerNotKnown :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    String →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerNotKnown action worker_id = do
    Set.member worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown action worker_id)
-- }}}

-- }}}
