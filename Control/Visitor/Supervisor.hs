-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- needed to define the MTL instances :-/
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Supervisor -- {{{
    ( RunStatistics(..)
    , SupervisorCallbacks(..)
    , SupervisorMonad
    , SupervisorMonadConstraint
    , SupervisorOutcome(..)
    , SupervisorProgram(..)
    , SupervisorTerminationReason(..)
    , SupervisorWorkerIdConstraint
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
    , runSupervisor
    , runSupervisorMaybeStartingFrom
    , runSupervisorStartingFrom
    , runUnrestrictedSupervisor
    , runUnrestrictedSupervisorMaybeStartingFrom
    , runUnrestrictedSupervisorStartingFrom
    , setSupervisorDebugMode
    ) where -- }}}

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative ((<$>),(<*>),Applicative)
import Control.Arrow (first,second)
import Control.Category ((>>>))
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),Exception(..),assert)
import Control.Lens.Getter (use)
import Control.Lens.Setter ((.=),(%=),(+=))
import Control.Lens.Lens ((<<%=))
import Control.Lens.TH (makeLenses)
import Control.Monad (forever,liftM,liftM2,mplus,unless,when)
import Control.Monad.CatchIO (MonadCatchIO,catch,throw)
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Control.Monad.Reader.Class as MTL
import qualified Control.Monad.State.Class as MTL
import Control.Monad.Reader (ask,asks)
import Control.Monad.Tools (ifM,whenM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Abort (AbortT(..),abort,runAbortT,unwrapAbortT)
import Control.Monad.Trans.Abort.Instances.MTL
import Control.Monad.Trans.Reader (ReaderT,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalStateT,execState,runStateT)

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
import Data.Time.Clock (NominalDiffTime,UTCTime,diffUTCTime,getCurrentTime)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor.Checkpoint
import Control.Visitor.Path
import Control.Visitor.Worker
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Exceptions {{{

-- InconsistencyError {{{
data InconsistencyError = -- {{{
    IncompleteWorkspace Checkpoint
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
  | ConflictingWorkloads (Maybe worker_id) Path (Maybe worker_id) Path
  | SpaceFullyExploredButWorkloadsRemain [(Maybe worker_id,Workload)]
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

data OccupationStatistics = OccupationStatistics -- {{{
    {   _last_occupied_change_time :: !UTCTime
    ,   _total_occupied_time :: !NominalDiffTime
    ,   _is_currently_occupied :: !Bool
    }
$( makeLenses ''OccupationStatistics )
-- }}}

data SupervisorCallbacks result worker_id m = -- {{{
    SupervisorCallbacks
    {   broadcastProgressUpdateToWorkers :: [worker_id] → m ()
    ,   broadcastWorkloadStealToWorkers :: [worker_id] → m ()
    ,   receiveCurrentProgress :: Progress result → m ()
    ,   sendWorkloadToWorker :: Workload → worker_id → m ()
    }
-- }}}

data SupervisorRunConstants result worker_id m = -- {{{
    SupervisorRunConstants
    {   callbacks :: !(SupervisorCallbacks result worker_id m)
    ,   start_time :: !UTCTime
    }
-- }}}

data SupervisorState result worker_id = -- {{{
    SupervisorState
    {   _waiting_workers_or_available_workloads :: !(Either (Set worker_id) (Set Workload))
    ,   _known_workers :: !(Set worker_id)
    ,   _active_workers :: !(Map worker_id Workload)
    ,   _current_steal_depth :: !Int
    ,   _available_workers_for_steal :: !(IntMap (Set worker_id))
    ,   _workers_pending_workload_steal :: !(Set worker_id)
    ,   _workers_pending_progress_update :: !(Set worker_id)
    ,   _current_progress :: !(Progress result)
    ,   _debug_mode :: !Bool
    ,   _supervisor_occupation_statistics :: OccupationStatistics
    }
$( makeLenses ''SupervisorState )
-- }}}

data SupervisorTerminationReason result worker_id = -- {{{
    SupervisorAborted (Progress result)
  | SupervisorCompleted result
  | SupervisorFailure worker_id String
  deriving (Eq,Show)
-- }}}

data SupervisorOutcome result worker_id = -- {{{
    SupervisorOutcome
    {   supervisorTerminationReason :: SupervisorTerminationReason result worker_id
    ,   supervisorRunStatistics :: RunStatistics
    ,   supervisorRemainingWorkers :: [worker_id]
    } deriving (Eq,Show)
-- }}}

type SupervisorContext result worker_id m = -- {{{
    StateT (SupervisorState result worker_id)
        (ReaderT (SupervisorRunConstants result worker_id m) m)
-- }}}

type SupervisorAbortMonad result worker_id m = -- {{{
    AbortT
        (SupervisorOutcome result worker_id)
        (SupervisorContext result worker_id m)
-- }}}

newtype SupervisorMonad result worker_id m α = -- {{{
    SupervisorMonad {
        unwrapSupervisorMonad :: SupervisorAbortMonad result worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

data WaitingSubprogram m α = -- {{{
    BlockingSubprogram (m α)
  | PollingSubprogram (m (Maybe α))
-- }}}

data SupervisorProgram result worker_id m = -- {{{
    ∀ α. BlockingProgram (SupervisorMonad result worker_id m ()) (m α) (α → SupervisorMonad result worker_id m ())
  | ∀ α. PollingProgram (SupervisorMonad result worker_id m ()) (m (Maybe α)) (α → SupervisorMonad result worker_id m ())
  | UnrestrictedProgram (∀ α. SupervisorMonad result worker_id m α)
-- }}}

data RunStatistics = -- {{{
    RunStatistics
    {   runStartTime :: !UTCTime
    ,   runEndTime :: !UTCTime
    ,   runWallTime :: !NominalDiffTime
    ,   runSupervisorOccupation :: !Float
    } deriving (Eq,Show)
-- }}}

-- }}}

-- Lens aliases {{{
supervisor_is_currently_occupied = supervisor_occupation_statistics . is_currently_occupied
supervisor_last_occupied_change_time = supervisor_occupation_statistics . last_occupied_change_time
supervisor_total_occupied_time = supervisor_occupation_statistics . total_occupied_time
-- }}}

-- Contraints {{{
type SupervisorMonadConstraint m = (Functor m, MonadCatchIO m)
type SupervisorWorkerIdConstraint worker_id = (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id)
-- }}}

-- Instances {{{

instance MonadTrans (SupervisorMonad result worker_id) where -- {{{
    lift = SupervisorMonad . lift . liftUserToContext
-- }}}

instance MTL.MonadReader r m ⇒ MTL.MonadReader r (SupervisorMonad result worker_id m) where -- {{{
    ask = lift MTL.ask
    local f m = SupervisorMonad $ do
        actions ← MTL.ask
        old_state ← MTL.get
        (result,new_state) ←
            lift
            .
            lift
            .
            lift
            .
            MTL.local f
            .
            flip runReaderT actions
            .
            flip runStateT old_state
            .
            unwrapAbortT
            .
            unwrapSupervisorMonad
            $
            m
        MTL.put new_state
        either abort return result
-- }}}

instance MTL.MonadState s m ⇒ MTL.MonadState s (SupervisorMonad result worker_id m) where -- {{{
    get = lift MTL.get
    put = lift . MTL.put
-- }}}

-- }}}

-- Exposed functions {{{

abortSupervisor :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m α -- {{{
abortSupervisor = SupervisorMonad $
    use current_progress >>= abortSupervisorWithReason . SupervisorAborted
-- }}}

addWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad result worker_id m ()
addWorker worker_id = postValidate ("addWorker " ++ show worker_id) . SupervisorMonad . lift $ do
    infoM $ "Adding worker " ++ show worker_id
    validateWorkerNotKnown "adding worker" worker_id
    known_workers %= Set.insert worker_id
    tryToObtainWorkloadFor worker_id
-- }}}

changeSupervisorOccupiedStatus :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad result worker_id m () -- {{{
changeSupervisorOccupiedStatus new_occupied_status = SupervisorMonad . lift $
    (/= new_occupied_status) <$> use supervisor_is_currently_occupied
    >>=
    flip when (
        liftIO getCurrentTime >>= \current_time →
        supervisor_occupation_statistics %= execState (do
            is_currently_occupied .= new_occupied_status
            last_time ← last_occupied_change_time <<%= const current_time
            unless new_occupied_status $ total_occupied_time += (current_time `diffUTCTime` last_time)
        )
    )
-- }}}

disableSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m () -- {{{
disableSupervisorDebugMode = setSupervisorDebugMode False
-- }}}

enableSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m () -- {{{
enableSupervisorDebugMode = setSupervisorDebugMode True
-- }}}

endSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m () -- {{{
endSupervisorOccupied = changeSupervisorOccupiedStatus False
-- }}}

getCurrentProgress :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m (Progress result) -- {{{
getCurrentProgress = SupervisorMonad . lift . use $ current_progress
-- }}}

getCurrentStatistics :: -- {{{
    SupervisorMonadConstraint m ⇒
    SupervisorMonad result worker_id m RunStatistics
getCurrentStatistics = do
    endSupervisorOccupied
    SupervisorMonad . lift $ do
        runEndTime ← liftIO getCurrentTime
        runStartTime ← asks start_time
        let runWallTime = runEndTime `diffUTCTime` runStartTime
        runSupervisorOccupation ←
            fromRational
            .
            toRational
            .
            (/ runWallTime)
            <$>
            use supervisor_total_occupied_time
        return RunStatistics{..}
-- }}}

getNumberOfWorkers :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m Int -- {{{
getNumberOfWorkers = SupervisorMonad . lift . liftM Set.size . use $ known_workers
-- }}}

getWaitingWorkers :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad result worker_id m (Set worker_id)
getWaitingWorkers = SupervisorMonad . lift $
    either id (const Set.empty) <$> use waiting_workers_or_available_workloads
-- }}}

performGlobalProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad result worker_id m ()
performGlobalProgressUpdate = postValidate "performGlobalProgressUpdate" . SupervisorMonad . lift $ do
    infoM $ "Performing global progress update."
    active_worker_ids ← Map.keysSet <$> use active_workers
    if (Set.null active_worker_ids)
        then sendCurrentProgressToUser
        else do
            workers_pending_progress_update .= active_worker_ids
            asks (callbacks >>> broadcastProgressUpdateToWorkers) >>= liftUserToContext . ($ Set.toList active_worker_ids)
-- }}}

receiveProgressUpdate :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdate result →
    SupervisorMonad result worker_id m ()
receiveProgressUpdate worker_id (ProgressUpdate progress_update remaining_workload) = postValidate ("receiveProgressUpdate " ++ show worker_id ++ " ...") . SupervisorMonad . lift $ do
    infoM $ "Received progress update from " ++ show worker_id
    validateWorkerKnownAndActive "receiving progress update" worker_id
    current_progress %= (`mappend` progress_update)
    is_pending_workload_steal ← Set.member worker_id <$> use workers_pending_workload_steal
    unless is_pending_workload_steal $ dequeueWorkerForSteal worker_id
    active_workers %= Map.insert worker_id remaining_workload
    unless is_pending_workload_steal $ enqueueWorkerForSteal worker_id
    clearPendingProgressUpdate worker_id
-- }}}

receiveStolenWorkload :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Maybe (StolenWorkload result) →
    SupervisorMonad result worker_id m ()
receiveStolenWorkload worker_id maybe_stolen_workload = postValidate ("receiveStolenWorkload " ++ show worker_id ++ " ...") . SupervisorMonad . lift $ do
    infoM $ "Received stolen workload from " ++ show worker_id
    validateWorkerKnownAndActive "receiving stolen workload" worker_id
    workers_pending_workload_steal %= Set.delete worker_id
    case maybe_stolen_workload of
        Nothing → return ()
        Just (StolenWorkload (ProgressUpdate progress_update remaining_workload) workload) → do
            current_progress %= (`mappend` progress_update)
            active_workers %= Map.insert worker_id remaining_workload
            enqueueWorkload workload
    enqueueWorkerForSteal worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

receiveWorkerFailure :: SupervisorMonadConstraint m ⇒ worker_id → String → SupervisorMonad result worker_id m α -- {{{
receiveWorkerFailure =
    (SupervisorMonad . abortSupervisorWithReason)
    .*
    SupervisorFailure
-- }}}

receiveWorkerFinished :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Progress result →
    SupervisorMonad result worker_id m ()
receiveWorkerFinished = receiveWorkerFinishedWithRemovalFlag False
-- }}}

receiveWorkerFinishedAndRemoved :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Progress result →
    SupervisorMonad result worker_id m ()
receiveWorkerFinishedAndRemoved = receiveWorkerFinishedWithRemovalFlag True
-- }}}

receiveWorkerFinishedWithRemovalFlag :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    Progress result →
    SupervisorMonad result worker_id m ()
receiveWorkerFinishedWithRemovalFlag remove_worker worker_id final_progress = postValidate ("receiveWorkerFinished " ++ show worker_id ++ " " ++ show (progressCheckpoint final_progress)) . SupervisorMonad $ do
    infoM $ if remove_worker
        then "Worker " ++ show worker_id ++ " finished and removed."
        else "Worker " ++ show worker_id ++ " finished."
    lift $ validateWorkerKnownAndActive "the worker was declared finished" worker_id
    current_progress %= (`mappend` final_progress)
    when remove_worker $ known_workers %= Set.delete worker_id
    Progress checkpoint new_results ← use current_progress
    case checkpoint of
        Explored → do
            active_worker_ids ← Map.keys . Map.delete worker_id <$> use active_workers
            unless (null active_worker_ids) . throw $
                ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
            SupervisorOutcome
                <$> (return $ SupervisorCompleted new_results)
                <*> (unwrapSupervisorMonad getCurrentStatistics)
                <*> (Set.toList <$> use known_workers)
             >>= abort
        _ → lift $ do
            deactivateWorker False worker_id
            unless remove_worker $ tryToObtainWorkloadFor worker_id
-- }}}

removeWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad result worker_id m ()
removeWorker worker_id = postValidate ("removeWorker " ++ show worker_id) . SupervisorMonad . lift $ do
    infoM $ "Removing worker " ++ show worker_id
    validateWorkerKnown "removing the worker" worker_id
    known_workers %= Set.delete worker_id
    ifM (isJust . Map.lookup worker_id <$> use active_workers)
        (deactivateWorker True worker_id)
        (waiting_workers_or_available_workloads %= either (Left . Set.delete worker_id) Right)
-- }}}

removeWorkerIfPresent :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad result worker_id m ()
removeWorkerIfPresent worker_id = postValidate ("removeWorker " ++ show worker_id) . SupervisorMonad . lift $ do
    whenM (Set.member worker_id <$> use known_workers) $ do
        infoM $ "Removing worker " ++ show worker_id
        known_workers %= Set.delete worker_id
        ifM (isJust . Map.lookup worker_id <$> use active_workers)
            (deactivateWorker True worker_id)
            (waiting_workers_or_available_workloads %= either (Left . Set.delete worker_id) Right)
-- }}}

runSupervisor :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorCallbacks result worker_id m →
    SupervisorProgram result worker_id m →
    m (SupervisorOutcome result worker_id)
runSupervisor = runSupervisorStartingFrom mempty
-- }}}

runSupervisorMaybeStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Maybe (Progress result) →
    SupervisorCallbacks result worker_id m →
    SupervisorProgram result worker_id m →
    m (SupervisorOutcome result worker_id)
runSupervisorMaybeStartingFrom Nothing = runSupervisor
runSupervisorMaybeStartingFrom (Just progress) = runSupervisorStartingFrom progress
-- }}}

runSupervisorStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Progress result →
    SupervisorCallbacks result worker_id m →
    SupervisorProgram result worker_id m →
    m (SupervisorOutcome result worker_id)
runSupervisorStartingFrom starting_progress actions program = liftIO getCurrentTime >>= \start_time →
    flip runReaderT (SupervisorRunConstants actions start_time)
    .
    flip evalStateT
        (SupervisorState
            {   _waiting_workers_or_available_workloads =
                    Right . Set.singleton $ Workload Seq.empty (progressCheckpoint starting_progress)
            ,   _known_workers = mempty
            ,   _active_workers = mempty
            ,   _current_steal_depth = 0
            ,   _available_workers_for_steal = mempty
            ,   _workers_pending_workload_steal = mempty
            ,   _workers_pending_progress_update = mempty
            ,   _current_progress = starting_progress
            ,   _debug_mode = False
            ,   _supervisor_occupation_statistics = OccupationStatistics
                {   _last_occupied_change_time = start_time
                ,   _total_occupied_time = 0
                ,   _is_currently_occupied = False
                }
            }
        )
    .
    runAbortT
    .
    (
        AbortT
        .
        (flip catch $ \e →
            let abortIt = unwrapAbortT . unwrapSupervisorMonad $ abortSupervisor
            in case fromException e of
                Just ThreadKilled → abortIt
                Just UserInterrupt → abortIt
                _ → throw e
        )
        .
        unwrapAbortT
    )
    .
    unwrapSupervisorMonad
    .
    runSupervisorProgram
    $
    program
-- }}}

runUnrestrictedSupervisor :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorCallbacks result worker_id m →
    (∀ α. SupervisorMonad result worker_id m α) →
    m (SupervisorOutcome result worker_id)
runUnrestrictedSupervisor callbacks = runSupervisorStartingFrom mempty callbacks . UnrestrictedProgram
-- }}}

runUnrestrictedSupervisorMaybeStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Maybe (Progress result) →
    SupervisorCallbacks result worker_id m →
    (∀ α. SupervisorMonad result worker_id m α) →
    m (SupervisorOutcome result worker_id)
runUnrestrictedSupervisorMaybeStartingFrom Nothing = runUnrestrictedSupervisor
runUnrestrictedSupervisorMaybeStartingFrom (Just progress) = runUnrestrictedSupervisorStartingFrom progress
-- }}}

runUnrestrictedSupervisorStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Progress result →
    SupervisorCallbacks result worker_id m →
    (∀ α. SupervisorMonad result worker_id m α) →
    m (SupervisorOutcome result worker_id)
runUnrestrictedSupervisorStartingFrom starting_progress actions =
    runSupervisorStartingFrom starting_progress actions . UnrestrictedProgram
-- }}}

setSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad result worker_id m () -- {{{
setSupervisorDebugMode = SupervisorMonad . lift . (debug_mode .=)
-- }}}

startSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m () -- {{{
startSupervisorOccupied = changeSupervisorOccupiedStatus True
-- }}}

-- }}}

-- Internal Functions {{{

abortSupervisorWithReason ::  -- {{{
    SupervisorMonadConstraint m ⇒
    SupervisorTerminationReason result worker_id →
    SupervisorAbortMonad result worker_id m α
abortSupervisorWithReason reason =
    (SupervisorOutcome
        <$> (return reason)
        <*> (unwrapSupervisorMonad getCurrentStatistics)
        <*> (Set.toList <$> use known_workers)
    ) >>= abort
-- }}}

checkWhetherMoreStealsAreNeeded :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorContext result worker_id m ()
checkWhetherMoreStealsAreNeeded = do
    number_of_waiting_workers ← either Set.size (const 0) <$> use waiting_workers_or_available_workloads
    number_of_pending_workload_steals ← Set.size <$> use workers_pending_workload_steal
    available_workers ← use available_workers_for_steal
    when (number_of_pending_workload_steals == 0
       && number_of_waiting_workers > 0
       && IntMap.null available_workers
      ) $ throw OutOfSourcesForNewWorkloads
    let number_of_needed_steals = (number_of_waiting_workers - number_of_pending_workload_steals) `max` 0
    when (number_of_needed_steals > 0 && (not . IntMap.null) available_workers) $ do
        depth ← do
            old_depth ← use current_steal_depth
            if number_of_pending_workload_steals == 0 && IntMap.notMember old_depth available_workers
                then do
                    let new_depth = fst (IntMap.findMin available_workers)
                    current_steal_depth .= new_depth
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
        workers_pending_workload_steal %= (Set.union . Set.fromList) workers_to_steal_from
        available_workers_for_steal %= IntMap.update (const maybe_new_workers) depth
        unless (null workers_to_steal_from) $ do
            infoM $ "Sending workload steal requests to " ++ show workers_to_steal_from
            asks (callbacks >>> broadcastWorkloadStealToWorkers) >>= liftUserToContext . ($ workers_to_steal_from)
-- }}}

clearPendingProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorContext result worker_id m ()
clearPendingProgressUpdate worker_id =
    Set.member worker_id <$> use workers_pending_progress_update >>= flip when
    -- Note, the conditional above is needed to prevent a "misfire" where
    -- we think that we have just completed a progress update even though
    -- none was started.
    (do workers_pending_progress_update %= Set.delete worker_id
        no_progress_updates_are_pending ← Set.null <$> use workers_pending_progress_update
        when no_progress_updates_are_pending sendCurrentProgressToUser
    )
-- }}}

deactivateWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    SupervisorContext result worker_id m ()
deactivateWorker reenqueue_workload worker_id = do
    workers_pending_workload_steal %= Set.delete worker_id
    dequeueWorkerForSteal worker_id
    if reenqueue_workload
        then active_workers <<%= Map.delete worker_id
              >>=
                enqueueWorkload
                .
                fromMaybe (error $ "Attempt to deactive worker " ++ show worker_id ++ " which was not listed as active.")
                .
                Map.lookup worker_id
        else active_workers %= Map.delete worker_id
    clearPendingProgressUpdate worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

dequeueWorkerForSteal :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorContext result worker_id m ()
dequeueWorkerForSteal worker_id =
    getWorkerDepth worker_id >>= \depth →
        available_workers_for_steal %=
            IntMap.update
                (\queue → let new_queue = Set.delete worker_id queue
                          in if Set.null new_queue then Nothing else Just new_queue
                )
                depth
-- }}}

enqueueWorkerForSteal :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorContext result worker_id m ()
enqueueWorkerForSteal worker_id =
    getWorkerDepth worker_id >>= \depth →
        available_workers_for_steal %=
            IntMap.alter
                (Just . maybe (Set.singleton worker_id) (Set.insert worker_id))
                depth
-- }}}

enqueueWorkload :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Workload →
    SupervisorContext result worker_id m ()
enqueueWorkload workload =
    use waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left (Set.minView → Just (free_worker_id,remaining_workers)) → do
            waiting_workers_or_available_workloads .= Left remaining_workers
            sendWorkloadTo workload free_worker_id
        Left (Set.minView → Nothing) →
            waiting_workers_or_available_workloads .= Right (Set.singleton workload)
        Right available_workloads →
            waiting_workers_or_available_workloads .= Right (Set.insert workload available_workloads)
-- }}}

getWorkerDepth :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorContext result worker_id m Int
getWorkerDepth worker_id =
    maybe
        (error $ "Attempted to use the depth of inactive worker " ++ show worker_id ++ ".")
        workloadDepth
    .
    Map.lookup worker_id
    <$>
    use active_workers
-- }}}

liftUserToContext :: Monad m ⇒ m α → SupervisorContext result worker_id m α -- {{{
liftUserToContext = lift . lift
-- }}}

postValidate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    String →
    SupervisorMonad result worker_id m α →
    SupervisorMonad result worker_id m α
postValidate label action = action >>= \result → SupervisorMonad . lift $
  (whenDebugging $ do
    debugM $ " === BEGIN VALIDATE === " ++ label
    use known_workers >>= debugM . ("Known workers is now " ++) . show
    use active_workers >>= debugM . ("Active workers is now " ++) . show
    use waiting_workers_or_available_workloads >>= debugM . ("Waiting/Available queue is now " ++) . show
    use current_progress >>= debugM . ("Current checkpoint is now " ++) . show . progressCheckpoint
    workers_and_workloads ←
        liftM2 (++)
            (map (Nothing,) . Set.toList . either (const (Set.empty)) id <$> use waiting_workers_or_available_workloads)
            (map (first Just) . Map.assocs <$> use active_workers)
    let go [] _ = return ()
        go ((maybe_worker_id,Workload initial_path _):rest_workloads) known_prefixes =
            case Map.lookup initial_path_as_list known_prefixes of
                Nothing → go rest_workloads . mappend known_prefixes . Map.fromList . map (,(maybe_worker_id,initial_path)) . inits $ initial_path_as_list
                Just (maybe_other_worker_id,other_initial_path) →
                    throw $ ConflictingWorkloads maybe_worker_id initial_path maybe_other_worker_id other_initial_path
          where initial_path_as_list = Fold.toList initial_path
    go workers_and_workloads Map.empty
    Progress checkpoint _ ← use current_progress
    let total_workspace =
            mappend checkpoint
            .
            mconcat
            .
            map (flip checkpointFromInitialPath Explored . workloadPath . snd)
            $
            workers_and_workloads
    unless (total_workspace == Explored) $ throw $ IncompleteWorkspace total_workspace
    Progress checkpoint _ ← use current_progress
    when (checkpoint == Explored) $
        if null workers_and_workloads
            then throw $ SpaceFullyExploredButSearchNotTerminated
            else throw $ SpaceFullyExploredButWorkloadsRemain workers_and_workloads
    debugM $ " === END VALIDATE === " ++ label
  ) >> return result
-- }}}

runSupervisorProgram :: SupervisorMonadConstraint m ⇒ SupervisorProgram result worker_id m → SupervisorMonad result worker_id m α -- {{{
runSupervisorProgram program =
    case program of
        BlockingProgram initialize getRequest processRequest → forever $ do
            initialize
            request ← lift getRequest
            startSupervisorOccupied
            processRequest request
            endSupervisorOccupied
        PollingProgram initialize getMaybeRequest processRequest → initialize >> forever (do
            maybe_request ← lift getMaybeRequest
            case maybe_request of
                Nothing → endSupervisorOccupied
                Just request → do
                    startSupervisorOccupied
                    processRequest request
         )
        UnrestrictedProgram run → run
-- }}}

sendCurrentProgressToUser :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorContext result worker_id m ()
sendCurrentProgressToUser = do
    callback ← asks (callbacks >>> receiveCurrentProgress)
    current_progress ← use current_progress
    liftUserToContext (callback current_progress)
-- }}}

sendWorkloadTo :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Workload →
    worker_id →
    SupervisorContext result worker_id m ()
sendWorkloadTo workload worker_id = do
    infoM $ "Sending workload to " ++ show worker_id
    asks (callbacks >>> sendWorkloadToWorker) >>= liftUserToContext . (\f → f workload worker_id)
    isNothing . Map.lookup worker_id <$> use active_workers
        >>= flip unless (throw $ WorkerAlreadyHasWorkload worker_id)
    active_workers %= Map.insert worker_id workload
    enqueueWorkerForSteal worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

tryToObtainWorkloadFor :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorContext result worker_id m ()
tryToObtainWorkloadFor worker_id =
    use waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers → do
            waiting_workers_or_available_workloads .= Left (Set.insert worker_id waiting_workers)
            checkWhetherMoreStealsAreNeeded
        Right (Set.minView → Nothing) → do
            waiting_workers_or_available_workloads .= Left (Set.singleton worker_id)
            checkWhetherMoreStealsAreNeeded
        Right (Set.minView → Just (workload,remaining_workloads)) → do
            sendWorkloadTo workload worker_id
            waiting_workers_or_available_workloads .= Right remaining_workloads
-- }}}

validateWorkerKnown :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    String →
    worker_id →
    SupervisorContext result worker_id m ()
validateWorkerKnown action worker_id =
    Set.notMember worker_id <$> (use known_workers)
        >>= flip when (throw $ WorkerNotKnown action worker_id)
-- }}}

validateWorkerKnownAndActive :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    String →
    worker_id →
    SupervisorContext result worker_id m ()
validateWorkerKnownAndActive action worker_id = do
    validateWorkerKnown action worker_id
    Set.notMember worker_id <$> (use known_workers)
        >>= flip when (throw $ WorkerNotActive action worker_id)
-- }}}

validateWorkerNotKnown :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    String →
    worker_id →
    SupervisorContext result worker_id m ()
validateWorkerNotKnown action worker_id = do
    Set.member worker_id <$> (use known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown action worker_id)
-- }}}

whenDebugging :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorContext result worker_id m () →
    SupervisorContext result worker_id m ()
whenDebugging action = use debug_mode >>= flip when action
-- }}}

-- }}}
