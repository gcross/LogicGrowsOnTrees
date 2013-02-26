-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Supervisor.Implementation -- {{{
    ( AbortMonad()
    , ContextMonad()
    , RunStatistics(..)
    , Statistics(..)
    , SupervisorCallbacks(..)
    , SupervisorFullConstraint
    , SupervisorMonadConstraint
    , SupervisorOutcome(..)
    , SupervisorTerminationReason(..)
    , SupervisorWorkerIdConstraint
    , TimeStatistics(..)
    , abortSupervisor
    , abortSupervisorWithReason
    , addWorker
    , changeSupervisorOccupiedStatus
    , current_time
    , getCurrentProgress
    , getCurrentStatistics
    , getNumberOfWorkers
    , liftContextToAbort
    , liftUserToAbort
    , localWithinAbort
    , localWithinContext
    , performGlobalProgressUpdate
    , receiveProgressUpdate
    , receiveStolenWorkload
    , receiveWorkerFinishedWithRemovalFlag
    , removeWorker
    , removeWorkerIfPresent
    , runSupervisorStartingFrom
    , setSupervisorDebugMode
    , time_spent_in_supervisor_monad
    , tryGetWaitingWorker
    ) where -- }}}

-- Imports {{{
import Control.Applicative ((<$>),(<*>),Applicative,liftA2)
import Control.Arrow ((&&&),first)
import Control.Category ((>>>))
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),Exception(..),assert,throw)
import Control.Lens ((&))
import Control.Lens.At (at)
import Control.Lens.Getter ((^.),use,view)
import Control.Lens.Setter ((.~),(+~),(.=),(%=),(+=))
import Control.Lens.Internal.Zoom (Zoomed)
import Control.Lens.Lens ((<%=),(<<%=),(<<.=),(%%=),(<<.=),Lens,Lens')
import Control.Lens.TH (makeLenses)
import Control.Lens.Zoom (Zoom(..))
import Control.Monad ((>=>),liftM,liftM2,mplus,unless,void,when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader (ask,asks)
import Control.Monad.Tools (ifM,whenM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Abort (AbortT(..),abort,runAbortT,unwrapAbortT)
import Control.Monad.Trans.Abort.Instances.MTL
import Control.Monad.Trans.Reader (ReaderT,runReader,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalState,evalStateT,execState,execStateT,runStateT)

import Data.Composition ((.*))
import Data.Derive.Monoid
import Data.DeriveTH
import Data.Either.Unwrap (whenLeft)
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (inits)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust,fromMaybe,isJust,isNothing)
import Data.Monoid ((<>),Monoid(..))
import Data.Monoid.Statistics (StatMonoid(..))
import Data.Monoid.Statistics.Numeric (CalcCount(..),CalcMean(..),CalcVariance(..),Min(..),Max(..),Variance(..),calcStddev)
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence ((><),Seq)
import Data.Typeable (Typeable)
import qualified Data.Time.Clock as Clock
import Data.Time.Clock (NominalDiffTime,UTCTime,diffUTCTime)

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

-- Statistics {{{

data ExponentiallyDecayingSum = ExponentiallyDecayingSum -- {{{
    {   _last_decaying_sum_timestamp :: !UTCTime
    ,   _decaying_sum_value :: !Float
    } deriving (Eq,Show)
$( makeLenses ''ExponentiallyDecayingSum )
-- }}}

data ExponentiallyWeightedAverage = ExponentiallyWeightedAverage -- {{{
    {   _last_average_timestamp :: !UTCTime
    ,   _current_average_value :: !Float
    } deriving (Eq,Show)
$( makeLenses ''ExponentiallyWeightedAverage )
-- }}}

data Statistics α = Statistics -- {{{
    {   statAverage :: !Double
    ,   statStdDev :: !Double
    ,   statMin :: !α
    ,   statMax :: !α
    } deriving (Eq,Show)
-- }}}

data RetiredOccupationStatistics = RetiredOccupationStatistics -- {{{
    {   _occupied_time :: !NominalDiffTime
    ,   _total_time :: !NominalDiffTime
    } deriving (Eq,Show)
$( makeLenses ''RetiredOccupationStatistics )
-- }}}

data OccupationStatistics = OccupationStatistics -- {{{
    {   _start_time :: !UTCTime
    ,   _last_occupied_change_time :: !UTCTime
    ,   _total_occupied_time :: !NominalDiffTime
    ,   _is_currently_occupied :: !Bool
    } deriving (Eq,Show)
$( makeLenses ''OccupationStatistics )
-- }}}

data RunStatistics = -- {{{
    RunStatistics
    {   runStartTime :: !UTCTime
    ,   runEndTime :: !UTCTime
    ,   runWallTime :: !NominalDiffTime
    ,   runSupervisorOccupation :: !Float
    ,   runSupervisorMonadOccupation :: !Float
    ,   runWorkerOccupation :: !Float
    ,   runWorkerWaitTimes :: !TimeStatistics
    ,   runStealWaitTimes :: !TimeStatistics
    ,   runWaitingWorkerStatistics :: !(Statistics Int)
    ,   runAvailableWorkloadStatistics :: !(Statistics Int)
    ,   runInstantaneousWorkloadRequestRateStatistics :: !(Statistics Float)
    ,   runInstantaneousWorkloadStealTimeStatistics :: !(Statistics Float)
    } deriving (Eq,Show)
-- }}}

data TimeStatistics = TimeStatistics -- {{{
    {   timeCount :: {-# UNPACK #-} !Int
    ,   timeMin :: {-# UNPACK #-} !Double
    ,   timeMax :: {-# UNPACK #-} !Double
    ,   timeMean :: {-# UNPACK #-} !Double
    ,   timeStdDev ::  {-# UNPACK #-} !Double
    } deriving (Eq,Show)
-- }}}

data TimeStatisticsMonoid = TimeStatisticsMonoid -- {{{
    {   timeDataMin :: {-# UNPACK #-} !Min
    ,   timeDataMax :: {-# UNPACK #-} !Max
    ,   timeDataVariance ::  {-# UNPACK #-} !Variance
    } deriving (Eq,Show)
$( derive makeMonoid ''TimeStatisticsMonoid )
-- }}}

data TimeWeightedStatistics α = TimeWeightedStatistics -- {{{
    {   _previous_value :: !α
    ,   _previous_time :: !UTCTime
    ,   _first_moment :: !Double
    ,   _second_moment :: !Double
    ,   _minimum_value :: !α
    ,   _maximum_value :: !α
    } deriving (Eq,Show)
$( makeLenses ''TimeWeightedStatistics )
-- }}}

-- }}}

data SupervisorCallbacks result worker_id m = -- {{{
    SupervisorCallbacks
    {   broadcastProgressUpdateToWorkers :: [worker_id] → m ()
    ,   broadcastWorkloadStealToWorkers :: [worker_id] → m ()
    ,   receiveCurrentProgress :: Progress result → m ()
    ,   sendWorkloadToWorker :: Workload → worker_id → m ()
    }
-- }}}

data SupervisorConstants result worker_id m = SupervisorConstants -- {{{
    {   callbacks :: !(SupervisorCallbacks result worker_id m)
    ,   _current_time :: UTCTime
    }
$( makeLenses ''SupervisorConstants )
-- }}}

data SupervisorState result worker_id = -- {{{
    SupervisorState
    {   _waiting_workers_or_available_workloads :: !(Either (Map worker_id (Maybe UTCTime)) (Set Workload))
    ,   _known_workers :: !(Set worker_id)
    ,   _active_workers :: !(Map worker_id Workload)
    ,   _available_workers_for_steal :: !(IntMap (Set worker_id))
    ,   _workers_pending_workload_steal :: !(Set worker_id)
    ,   _workers_pending_progress_update :: !(Set worker_id)
    ,   _current_progress :: !(Progress result)
    ,   _debug_mode :: !Bool
    ,   _supervisor_occupation_statistics :: OccupationStatistics
    ,   _worker_occupation_statistics :: !(Map worker_id OccupationStatistics)
    ,   _retired_worker_occupation_statistics :: !(Map worker_id RetiredOccupationStatistics)
    ,   _worker_wait_time_statistics :: !TimeStatisticsMonoid
    ,   _steal_request_matcher_queue :: !(MultiSet UTCTime)
    ,   _steal_request_failures :: !Int
    ,   _workload_steal_time_statistics :: !TimeStatisticsMonoid
    ,   _waiting_worker_count_statistics :: !(TimeWeightedStatistics Int)
    ,   _available_workload_count_statistics :: !(TimeWeightedStatistics Int)
    ,   _instantaneous_workload_request_rate :: !ExponentiallyDecayingSum
    ,   _instantaneous_workload_request_rate_statistics :: !(TimeWeightedStatistics Float)
    ,   _instantaneous_workload_steal_time :: !ExponentiallyWeightedAverage
    ,   _instantaneous_workload_steal_time_statistics :: !(TimeWeightedStatistics Float)
    ,   _time_spent_in_supervisor_monad :: !NominalDiffTime
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

type InsideContextMonad result worker_id m = -- {{{
    StateT (SupervisorState result worker_id) (
        ReaderT (SupervisorConstants result worker_id m)
            m
    )
-- }}}
newtype ContextMonad result worker_id m α = ContextMonad -- {{{
    { unwrapContextMonad :: InsideContextMonad result worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}
type ZoomedInStateContext result worker_id m s = StateT s (ReaderT (SupervisorCallbacks result worker_id m) m)

type InsideAbortMonad result worker_id m = -- {{{
    AbortT
        (SupervisorOutcome result worker_id)
        (ContextMonad result worker_id m)
-- }}}
newtype AbortMonad result worker_id m α = AbortMonad -- {{{
    { unwrapAbortMonad :: InsideAbortMonad result worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

-- }}}

-- Contraints {{{
type SupervisorReaderConstraint result worker_id m m' = MonadReader (SupervisorConstants result worker_id m) m'
type SupervisorStateConstraint result worker_id m' = MonadState (SupervisorState result worker_id) m'

type SupervisorMonadConstraint m = (Functor m, MonadIO m)
type SupervisorWorkerIdConstraint worker_id = (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id)
type SupervisorFullConstraint worker_id m = (SupervisorWorkerIdConstraint worker_id,SupervisorMonadConstraint m)
-- }}}

-- Instances {{{

type instance Zoomed (ContextMonad result worker_id m) = Zoomed (InsideContextMonad result worker_id m)

instance Monad m ⇒ MonadReader (SupervisorConstants result worker_id m) (AbortMonad result worker_id m) where -- {{{
    ask = AbortMonad ask
    local f = AbortMonad . local f . unwrapAbortMonad
-- }}}

instance Monad m ⇒ MonadReader (SupervisorConstants result worker_id m) (ContextMonad result worker_id m) where -- {{{
    ask = ContextMonad ask
    local f = ContextMonad . local f . unwrapContextMonad
-- }}}

instance Monad m ⇒ MonadState (SupervisorState result worker_id) (AbortMonad result worker_id m) where -- {{{
    get = AbortMonad get
    put = AbortMonad . put
-- }}}

instance Monad m ⇒ MonadState (SupervisorState result worker_id) (ContextMonad result worker_id m) where -- {{{
    get = ContextMonad get
    put = ContextMonad . put
-- }}}

instance Monoid RetiredOccupationStatistics where -- {{{
    mempty = RetiredOccupationStatistics 0 0
    mappend x y = x & (occupied_time +~ y^.occupied_time) & (total_time +~ y^.total_time)
-- }}}

instance StatMonoid TimeStatisticsMonoid NominalDiffTime where -- {{{
    pappend t =
        TimeStatisticsMonoid
            <$> (pappend t' . timeDataMin)
            <*> (pappend t' . timeDataMax)
            <*> (pappend t' . timeDataVariance)
      where t' = fromRational . toRational $ t :: Double
-- }}}

instance CalcCount TimeStatisticsMonoid where -- {{{
    calcCount = calcCount . timeDataVariance
-- }}}

instance CalcMean TimeStatisticsMonoid where -- {{{
    calcMean = calcMean . timeDataVariance
-- }}}

instance CalcVariance TimeStatisticsMonoid where -- {{{
    calcVariance = calcVariance . timeDataVariance
    calcVarianceUnbiased = calcVarianceUnbiased . timeDataVariance
-- }}}

-- }}}

-- Functions {{{

abortSupervisor :: SupervisorFullConstraint worker_id m ⇒ AbortMonad result worker_id m α -- {{{
abortSupervisor = use current_progress >>= abortSupervisorWithReason . SupervisorAborted
-- }}}

abortSupervisorWithReason ::  -- {{{
    SupervisorFullConstraint worker_id m ⇒
    SupervisorTerminationReason result worker_id →
    AbortMonad result worker_id m α
abortSupervisorWithReason reason = AbortMonad $
    (SupervisorOutcome
        <$> (return reason)
        <*>  getCurrentStatistics
        <*> (Set.toList <$> use known_workers)
    ) >>= abort
-- }}}

addPointToExponentiallyDecayingSum :: UTCTime → ExponentiallyDecayingSum → ExponentiallyDecayingSum -- {{{
addPointToExponentiallyDecayingSum current_time = execState $ do
    previous_time ← last_decaying_sum_timestamp <<.= current_time
    decaying_sum_value %= (+ 1) . (* computeExponentialDecayCoefficient previous_time current_time)
-- }}}

addPointToExponentiallyWeightedAverage :: Float → UTCTime → ExponentiallyWeightedAverage → ExponentiallyWeightedAverage -- {{{
addPointToExponentiallyWeightedAverage current_value current_time = execState $ do
    previous_time ← last_average_timestamp <<.= current_time
    let old_value_weight = exp . fromRational . toRational $ (previous_time `diffUTCTime` current_time)
        new_value_weight = 1 - old_value_weight
    current_average_value %= (+ new_value_weight * current_value) . (* old_value_weight)
-- }}}

addWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
addWorker worker_id = postValidate ("addWorker " ++ show worker_id) $ do
    infoM $ "Adding worker " ++ show worker_id
    validateWorkerNotKnown "adding worker" worker_id
    known_workers %= Set.insert worker_id
    start_time ← view current_time
    worker_occupation_statistics %= Map.insert worker_id (OccupationStatistics start_time start_time 0 False)
    tryToObtainWorkloadFor True worker_id
-- }}}

beginWorkerOccupied :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
beginWorkerOccupied = flip changeWorkerOccupiedStatus True
-- }}}

changeOccupiedStatus :: -- {{{
    ( Monad m'
    , SupervisorReaderConstraint result worker_id m m'
    ) ⇒
    m' OccupationStatistics →
    (OccupationStatistics → m' ()) →
    Bool →
    m' ()
changeOccupiedStatus getOccupation putOccupation new_occupied_status = do
    old_occupation ← getOccupation
    unless (old_occupation^.is_currently_occupied == new_occupied_status) $
        (flip execStateT old_occupation $ do
            current_time ← view current_time
            is_currently_occupied .= new_occupied_status
            last_time ← last_occupied_change_time <<.= current_time
            unless new_occupied_status $ total_occupied_time += (current_time `diffUTCTime` last_time)
        ) >>= putOccupation
-- }}}

changeSupervisorOccupiedStatus :: SupervisorMonadConstraint m ⇒ Bool → ContextMonad result worker_id m () -- {{{
changeSupervisorOccupiedStatus =
    changeOccupiedStatus
        (use supervisor_occupation_statistics)
        (supervisor_occupation_statistics .=)
-- }}}

changeWorkerOccupiedStatus :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Bool →
    ContextMonad result worker_id m ()
changeWorkerOccupiedStatus worker_id =
    changeOccupiedStatus
        (fromJust . Map.lookup worker_id <$> use worker_occupation_statistics)
        ((worker_occupation_statistics %=) . Map.insert worker_id)
-- }}}

checkWhetherMoreStealsAreNeeded :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ContextMonad result worker_id m ()
checkWhetherMoreStealsAreNeeded = do
    number_of_waiting_workers ← either Map.size (const 0) <$> use waiting_workers_or_available_workloads
    number_of_pending_workload_steals ← Set.size <$> use workers_pending_workload_steal
    available_workers ← use available_workers_for_steal
    when (number_of_pending_workload_steals == 0
       && number_of_waiting_workers > 0
       && IntMap.null available_workers
      ) $ throw OutOfSourcesForNewWorkloads
    let number_of_needed_steals = (number_of_waiting_workers - number_of_pending_workload_steals) `max` 0
    when (number_of_needed_steals > 0) $ do
        let findWorkers accum 0 available_workers = (accum,available_workers)
            findWorkers accum n (IntMap.minViewWithKey → Nothing) = (accum,IntMap.empty)
            findWorkers accum n (IntMap.minViewWithKey → Just ((depth,workers),deeper_workers)) =
                go accum n workers
              where
                go accum 0 workers = (accum,if Set.null workers then deeper_workers else IntMap.insert depth workers deeper_workers)
                go accum n (Set.minView → Nothing) = findWorkers accum n deeper_workers
                go accum n (Set.minView → Just (worker_id,rest_workers)) = go (worker_id:accum) (n-1) rest_workers
        workers_to_steal_from ← available_workers_for_steal %%= findWorkers [] number_of_needed_steals
        let number_of_workers_to_steal_from = length workers_to_steal_from
        original_steal_request_failures ← use steal_request_failures
        number_of_additional_requests ← steal_request_failures %%=
            \number_of_steal_request_failures →
                if number_of_steal_request_failures >= number_of_workers_to_steal_from
                    then (0,number_of_steal_request_failures-number_of_workers_to_steal_from)
                    else (number_of_workers_to_steal_from-number_of_steal_request_failures,0)
        current_time ← view current_time
        steal_request_matcher_queue %= (MultiSet.insertMany current_time number_of_additional_requests)
        workers_pending_workload_steal %= (Set.union . Set.fromList) workers_to_steal_from
        unless (null workers_to_steal_from) $ do
            infoM $ "Sending workload steal requests to " ++ show workers_to_steal_from
            asks (callbacks >>> broadcastWorkloadStealToWorkers) >>= liftUserToContext . ($ workers_to_steal_from)
-- }}}

clearPendingProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
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

computeExponentialDecayCoefficient :: UTCTime → UTCTime → Float -- {{{
computeExponentialDecayCoefficient = (exp . fromRational . toRational) .* diffUTCTime
-- }}}

computeInstantaneousRateFromDecayingSum :: -- {{{
    ( Functor m'
    , SupervisorMonadConstraint m
    , SupervisorReaderConstraint result worker_id m m'
    ) ⇒
    ExponentiallyDecayingSum →
    m' Float
computeInstantaneousRateFromDecayingSum expsum = do
    current_time ← view current_time
    flip runReaderT expsum $ do
        previous_time ← view last_decaying_sum_timestamp
        (* computeExponentialDecayCoefficient previous_time current_time) <$> view decaying_sum_value
-- }}}

deactivateWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    ContextMonad result worker_id m ()
deactivateWorker reenqueue_workload worker_id = do
    pending_steal ← workers_pending_workload_steal %%= (Set.member worker_id &&& Set.delete worker_id)
    when pending_steal $ steal_request_failures += 1
    dequeueWorkerForSteal worker_id
    if reenqueue_workload
        then active_workers <<%= Map.delete worker_id
              >>=
                enqueueWorkload
                .
                fromMaybe (error $ "Attempt to deactivate worker " ++ show worker_id ++ " which was not listed as active.")
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
    ContextMonad result worker_id m ()
dequeueWorkerForSteal worker_id =
    getWorkerDepth worker_id >>= \depth →
        available_workers_for_steal %=
            IntMap.update
                (\queue → let new_queue = Set.delete worker_id queue
                          in if Set.null new_queue then Nothing else Just new_queue
                )
                depth
-- }}}

endSupervisorOccupied :: SupervisorMonadConstraint m ⇒ ContextMonad result worker_id m () -- {{{
endSupervisorOccupied = changeSupervisorOccupiedStatus False
-- }}}

endWorkerOccupied :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
endWorkerOccupied = flip changeWorkerOccupiedStatus False
-- }}}

enqueueWorkerForSteal :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
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
    ContextMonad result worker_id m ()
enqueueWorkload workload =
    use waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left (Map.minViewWithKey → Just ((free_worker_id,maybe_time_started_waiting),remaining_workers)) → do
            waiting_workers_or_available_workloads .= Left remaining_workers
            maybe (return ())
                  (timePassedSince >=> (worker_wait_time_statistics %=) . pappend)
                  maybe_time_started_waiting
            sendWorkloadTo workload free_worker_id
            updateTimeWeightedStatisticsUsingLens waiting_worker_count_statistics (Map.size remaining_workers)
        Left (Map.minViewWithKey → Nothing) → do
            waiting_workers_or_available_workloads .= Right (Set.singleton workload)
            updateTimeWeightedStatisticsUsingLens available_workload_count_statistics 1
        Right available_workloads → do
            waiting_workers_or_available_workloads .= Right (Set.insert workload available_workloads)
            updateTimeWeightedStatisticsUsingLens available_workload_count_statistics (Set.size available_workloads + 1)
    >>
    checkWhetherMoreStealsAreNeeded
-- }}}

extractTimeStatistics :: TimeStatisticsMonoid → TimeStatistics -- {{{
extractTimeStatistics =
    TimeStatistics
        <$>  calcCount
        <*> (calcMin . timeDataMin)
        <*> (calcMax . timeDataMax)
        <*>  calcMean
        <*>  calcStddev
-- }}}

finalizeStatistics :: -- {{{
    ( Ord α
    , Real α
    , SupervisorMonadConstraint m
    , SupervisorMonadConstraint m'
    , SupervisorReaderConstraint result worker_id m m'
    ) ⇒
    UTCTime →
    m' α →
    m' (TimeWeightedStatistics α) →
    m' (Statistics α)
finalizeStatistics start_time getFinalValue getWeightedStatistics = do
    end_time ← view current_time
    let total_weight = fromRational . toRational $ (end_time `diffUTCTime` start_time)
    final_value ← getFinalValue
    getWeightedStatistics >>= (evalStateT $ do
        updateTimeWeightedStatistics final_value end_time
        statAverage ← (/total_weight) <$> use first_moment
        statStdDev ← sqrt . (\x → x-statAverage*statAverage) . (/total_weight) <$> use second_moment
        statMin ← min final_value <$> use minimum_value
        statMax ← max final_value <$> use maximum_value
        return $ Statistics{..}
     )
-- }}}

getCurrentProgress :: SupervisorMonadConstraint m ⇒ ContextMonad result worker_id m (Progress result) -- {{{
getCurrentProgress = use current_progress
-- }}}

getCurrentStatistics :: -- {{{
    ( SupervisorFullConstraint worker_id m
    , SupervisorMonadConstraint m'
    , SupervisorReaderConstraint result worker_id m m'
    , SupervisorStateConstraint result worker_id m'
    ) ⇒
    m' RunStatistics
getCurrentStatistics = do
    runEndTime ← view current_time
    runStartTime ← use (supervisor_occupation_statistics . start_time)
    let runWallTime = runEndTime `diffUTCTime` runStartTime
    runSupervisorOccupation ←
        use supervisor_occupation_statistics
        >>=
        retireOccupationStatistics
        >>=
        return . getOccupationFraction
    runSupervisorMonadOccupation ←
        fromRational
        .
        toRational
        .
        (/runWallTime)
        <$>
        use time_spent_in_supervisor_monad
    runWorkerOccupation ←
        getOccupationFraction . mconcat . Map.elems
        <$>
        liftM2 (Map.unionWith (<>))
            (use retired_worker_occupation_statistics)
            (use worker_occupation_statistics >>= retireManyOccupationStatistics)
    runWorkerWaitTimes ← extractTimeStatistics <$> use worker_wait_time_statistics
    runStealWaitTimes ← extractTimeStatistics <$> use workload_steal_time_statistics
    runWaitingWorkerStatistics ←
        finalizeStatistics
            runStartTime
            (either Map.size (const 0) <$> use waiting_workers_or_available_workloads)
            (use waiting_worker_count_statistics)
    runAvailableWorkloadStatistics ←
        finalizeStatistics
            runStartTime
            (either (const 0) Set.size <$> use waiting_workers_or_available_workloads)
            (use available_workload_count_statistics)
    runInstantaneousWorkloadRequestRateStatistics ←
        finalizeStatistics
            runStartTime
            (use instantaneous_workload_request_rate >>= computeInstantaneousRateFromDecayingSum)
            (use instantaneous_workload_request_rate_statistics)
    runInstantaneousWorkloadStealTimeStatistics ←
        finalizeStatistics
            runStartTime
            (use $ instantaneous_workload_steal_time . current_average_value)
            (use instantaneous_workload_steal_time_statistics)
    return RunStatistics{..}
-- }}}

getNumberOfWorkers :: SupervisorMonadConstraint m ⇒ ContextMonad result worker_id m Int -- {{{
getNumberOfWorkers = liftM Set.size . use $ known_workers
-- }}}

getOccupationFraction :: RetiredOccupationStatistics → Float -- {{{
getOccupationFraction = fromRational . toRational . liftA2 (/) (^.occupied_time) (^.total_time)
-- }}}

getWorkerDepth :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m Int
getWorkerDepth worker_id =
    maybe
        (error $ "Attempted to use the depth of inactive worker " ++ show worker_id ++ ".")
        workloadDepth
    .
    Map.lookup worker_id
    <$>
    use active_workers
-- }}}

initialTimeWeightedStatisticsForStartingTime :: Num α ⇒ UTCTime → TimeWeightedStatistics α -- {{{
initialTimeWeightedStatisticsForStartingTime starting_time =
    TimeWeightedStatistics
        0
        starting_time
        0
        0
        (fromIntegral (maxBound :: Int))
        (fromIntegral (minBound :: Int))
-- }}}

liftContextToAbort :: Monad m ⇒ ContextMonad result worker_id m α → AbortMonad result worker_id m α -- {{{
liftContextToAbort = AbortMonad . lift
-- }}}

liftUserToAbort :: Monad m ⇒ m α → AbortMonad result worker_id m α -- {{{
liftUserToAbort = liftContextToAbort . liftUserToContext
-- }}}

liftUserToContext :: Monad m ⇒ m α → ContextMonad result worker_id m α -- {{{
liftUserToContext = ContextMonad . lift . lift
-- }}}

localWithinAbort :: -- {{{
    MonadReader r m ⇒
    (r → r) →
    AbortMonad result worker_id m α →
    AbortMonad result worker_id m α
localWithinAbort f = AbortMonad . AbortT . localWithinContext f . unwrapAbortT . unwrapAbortMonad
-- }}}

localWithinContext :: -- {{{
    MonadReader r m ⇒
    (r → r) →
    ContextMonad result worker_id m α →
    ContextMonad result worker_id m α
localWithinContext f m = do
    actions ← ask
    old_state ← get
    (result,new_state) ←
        ContextMonad
        .
        lift
        .
        lift
        .
        local f
        .
        flip runReaderT actions
        .
        flip runStateT old_state
        .
        unwrapContextMonad
        $
        m
    put new_state
    return result
-- }}}

performGlobalProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ContextMonad result worker_id m ()
performGlobalProgressUpdate = postValidate "performGlobalProgressUpdate" $ do
    infoM $ "Performing global progress update."
    active_worker_ids ← Map.keysSet <$> use active_workers
    if (Set.null active_worker_ids)
        then sendCurrentProgressToUser
        else do
            workers_pending_progress_update .= active_worker_ids
            asks (callbacks >>> broadcastProgressUpdateToWorkers) >>= liftUserToContext . ($ Set.toList active_worker_ids)
-- }}}

postValidate :: -- {{{
    ( SupervisorMonadConstraint m'
    , SupervisorFullConstraint worker_id m
    , SupervisorStateConstraint result worker_id m'
    , SupervisorReaderConstraint result worker_id m m'
    ) ⇒
    String →
    m' α →
    m' α
postValidate label action = action >>= \result →
  (use debug_mode >>= flip when (do
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
  )) >> return result
-- }}}

receiveProgressUpdate :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdate result →
    ContextMonad result worker_id m ()
receiveProgressUpdate worker_id (ProgressUpdate progress_update remaining_workload) = postValidate ("receiveProgressUpdate " ++ show worker_id ++ " ...") $ do
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
    ContextMonad result worker_id m ()
receiveStolenWorkload worker_id maybe_stolen_workload = postValidate ("receiveStolenWorkload " ++ show worker_id ++ " ...") $ do
    infoM $ "Received stolen workload from " ++ show worker_id
    validateWorkerKnownAndActive "receiving stolen workload" worker_id
    workers_pending_workload_steal %= Set.delete worker_id
    case maybe_stolen_workload of
        Nothing → steal_request_failures += 1
        Just (StolenWorkload (ProgressUpdate progress_update remaining_workload) workload) → do
            (steal_request_matcher_queue %%= fromMaybe (error "Unable to find a request matching this steal!") . MultiSet.minView)
              >>= (timePassedSince >=> liftA2 (>>) ((workload_steal_time_statistics %=) . pappend) updateInstataneousWorkloadStealTime)
            current_progress %= (`mappend` progress_update)
            active_workers %= Map.insert worker_id remaining_workload
            enqueueWorkload workload
    enqueueWorkerForSteal worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

receiveWorkerFinishedWithRemovalFlag :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    Progress result →
    AbortMonad result worker_id m ()
receiveWorkerFinishedWithRemovalFlag remove_worker worker_id final_progress = AbortMonad . postValidate ("receiveWorkerFinished " ++ show worker_id ++ " " ++ show (progressCheckpoint final_progress)) $ do
    infoM $ if remove_worker
        then "Worker " ++ show worker_id ++ " finished and removed."
        else "Worker " ++ show worker_id ++ " finished."
    lift $ validateWorkerKnownAndActive "the worker was declared finished" worker_id
    Progress checkpoint new_results ← current_progress <%= (<> final_progress)
    when remove_worker . lift $ retireWorker worker_id
    case checkpoint of
        Explored → do
            active_worker_ids ← Map.keys . Map.delete worker_id <$> use active_workers
            unless (null active_worker_ids) . throw $
                ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
            SupervisorOutcome
                <$> (return $ SupervisorCompleted new_results)
                <*> (lift getCurrentStatistics)
                <*> (Set.toList <$> use known_workers)
             >>= abort
        _ → lift $ do
                deactivateWorker False worker_id
                unless remove_worker $ do
                    endWorkerOccupied worker_id
                    tryToObtainWorkloadFor False worker_id
-- }}}

retireManyOccupationStatistics :: -- {{{
    ( Functor f
    , SupervisorMonadConstraint m
    , Monad m'
    , SupervisorReaderConstraint result worker_id m m'
    ) ⇒
    f OccupationStatistics →
    m' (f RetiredOccupationStatistics)
retireManyOccupationStatistics occupied_statistics =
    view current_time >>= \current_time → return $
    fmap (\o → mempty
        & occupied_time .~ (o^.total_occupied_time + if o^.is_currently_occupied then current_time `diffUTCTime` (o^.last_occupied_change_time) else 0)
        & total_time .~ (current_time `diffUTCTime` (o^.start_time))
    ) occupied_statistics
-- }}}

retireOccupationStatistics :: -- {{{
    ( SupervisorMonadConstraint m
    , Monad m'
    , SupervisorReaderConstraint result worker_id m m'
    ) ⇒
    OccupationStatistics →
    m' RetiredOccupationStatistics
retireOccupationStatistics = liftM head . retireManyOccupationStatistics . (:[])
-- }}}

removeWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
removeWorker worker_id = postValidate ("removeWorker " ++ show worker_id) $ do
    infoM $ "Removing worker " ++ show worker_id
    validateWorkerKnown "removing the worker" worker_id
    retireAndDeactivateWorker worker_id
-- }}}

removeWorkerIfPresent :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
removeWorkerIfPresent worker_id = postValidate ("removeWorker " ++ show worker_id) $ do
    whenM (Set.member worker_id <$> use known_workers) $ do
        infoM $ "Removing worker " ++ show worker_id
        retireAndDeactivateWorker worker_id
-- }}}

retireWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
retireWorker worker_id = do
    known_workers %= Set.delete worker_id
    retired_occupation_statistics ←
        worker_occupation_statistics %%= (fromJust . Map.lookup worker_id &&& Map.delete worker_id)
        >>=
        retireOccupationStatistics
    retired_worker_occupation_statistics %= Map.alter (
        Just . maybe retired_occupation_statistics (<> retired_occupation_statistics)
     ) worker_id
-- }}}

retireAndDeactivateWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad result worker_id m ()
retireAndDeactivateWorker worker_id = do
    retireWorker worker_id
    ifM (isJust . Map.lookup worker_id <$> use active_workers)
        (deactivateWorker True worker_id)
        (waiting_workers_or_available_workloads %%=
            either (pred . Map.size &&& Left . Map.delete worker_id)
                   (error $ "worker " ++ show worker_id ++ " is inactive but was not in the waiting queue")
         >>= updateTimeWeightedStatisticsUsingLens waiting_worker_count_statistics
        )
-- }}}

runSupervisorStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Progress result →
    SupervisorCallbacks result worker_id m →
    (∀ α. AbortMonad result worker_id m α) →
    m (SupervisorOutcome result worker_id)
runSupervisorStartingFrom starting_progress actions program = liftIO Clock.getCurrentTime >>= \start_time →
    flip runReaderT (SupervisorConstants actions undefined)
    .
    flip evalStateT
        (SupervisorState
            {   _waiting_workers_or_available_workloads =
                    Right . Set.singleton $ Workload Seq.empty (progressCheckpoint starting_progress)
            ,   _known_workers = mempty
            ,   _active_workers = mempty
            ,   _available_workers_for_steal = mempty
            ,   _workers_pending_workload_steal = mempty
            ,   _workers_pending_progress_update = mempty
            ,   _current_progress = starting_progress
            ,   _debug_mode = False
            ,   _supervisor_occupation_statistics = OccupationStatistics
                {   _start_time = start_time
                ,   _last_occupied_change_time = start_time
                ,   _total_occupied_time = 0
                ,   _is_currently_occupied = False
                }
            ,   _worker_occupation_statistics = mempty
            ,   _retired_worker_occupation_statistics = mempty
            ,   _worker_wait_time_statistics = mempty
            ,   _steal_request_matcher_queue = mempty
            ,   _steal_request_failures = 0
            ,   _workload_steal_time_statistics = mempty
            ,   _waiting_worker_count_statistics = initialTimeWeightedStatisticsForStartingTime start_time
            ,   _available_workload_count_statistics = initialTimeWeightedStatisticsForStartingTime start_time
            ,   _instantaneous_workload_request_rate = ExponentiallyDecayingSum start_time 0
            ,   _instantaneous_workload_request_rate_statistics = initialTimeWeightedStatisticsForStartingTime start_time
            ,   _instantaneous_workload_steal_time = ExponentiallyWeightedAverage start_time 0
            ,   _instantaneous_workload_steal_time_statistics = initialTimeWeightedStatisticsForStartingTime start_time
            ,   _time_spent_in_supervisor_monad = 0
            }
        )
    .
    unwrapContextMonad
    .
    runAbortT
    .
    unwrapAbortMonad
    $
    program
-- }}}

sendCurrentProgressToUser :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ContextMonad result worker_id m ()
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
    ContextMonad result worker_id m ()
sendWorkloadTo workload worker_id = do
    infoM $ "Sending workload to " ++ show worker_id
    asks (callbacks >>> sendWorkloadToWorker) >>= liftUserToContext . (\f → f workload worker_id)
    isNothing . Map.lookup worker_id <$> use active_workers
        >>= flip unless (throw $ WorkerAlreadyHasWorkload worker_id)
    active_workers %= Map.insert worker_id workload
    beginWorkerOccupied worker_id
    enqueueWorkerForSteal worker_id
    checkWhetherMoreStealsAreNeeded
-- }}}

setSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ Bool → ContextMonad result worker_id m () -- {{{
setSupervisorDebugMode = (debug_mode .=)
-- }}}

timePassedSince :: -- {{{
    ( Functor m'
    , SupervisorMonadConstraint m
    , SupervisorReaderConstraint result worker_id m  m'
    ) ⇒
    UTCTime →
    m' NominalDiffTime
timePassedSince = (<$> view current_time) . flip diffUTCTime
-- }}}

tryGetWaitingWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ContextMonad result worker_id m (Maybe worker_id)
tryGetWaitingWorker =
    either (fmap (fst . fst) . Map.minViewWithKey) (const Nothing)
    <$>
    use waiting_workers_or_available_workloads
-- }}}

tryToObtainWorkloadFor :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    ContextMonad result worker_id m ()
tryToObtainWorkloadFor is_new_worker worker_id =
    unless is_new_worker updateInstataneousWorkloadRequestRate
    >>
    use waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers → do
            maybe_time_started_waiting ← getMaybeTimeStartedWorking
            waiting_workers_or_available_workloads .= Left (Map.insert worker_id maybe_time_started_waiting waiting_workers)
            updateTimeWeightedStatisticsUsingLens waiting_worker_count_statistics (Map.size waiting_workers + 1)
        Right (Set.minView → Nothing) → do
            maybe_time_started_waiting ← getMaybeTimeStartedWorking
            waiting_workers_or_available_workloads .= Left (Map.singleton worker_id maybe_time_started_waiting)
            updateTimeWeightedStatisticsUsingLens waiting_worker_count_statistics 1
        Right (Set.minView → Just (workload,remaining_workloads)) → do
            sendWorkloadTo workload worker_id
            waiting_workers_or_available_workloads .= Right remaining_workloads
            updateTimeWeightedStatisticsUsingLens available_workload_count_statistics (Set.size remaining_workloads + 1)
    >>
    checkWhetherMoreStealsAreNeeded
  where
    getMaybeTimeStartedWorking
      | is_new_worker = return Nothing
      | otherwise = Just <$> view current_time

-- }}}

updateInstataneousWorkloadRequestRate :: SupervisorMonadConstraint m ⇒ ContextMonad result worker_id m () -- {{{
updateInstataneousWorkloadRequestRate = do
    current_time ← view current_time
    previous_value ← instantaneous_workload_request_rate %%= ((^.decaying_sum_value) &&& addPointToExponentiallyDecayingSum current_time)
    current_value ← use (instantaneous_workload_request_rate . decaying_sum_value)
    updateTimeWeightedStatisticsUsingLens instantaneous_workload_request_rate_statistics ((current_value + previous_value) / 2)
-- }}}

updateInstataneousWorkloadStealTime :: SupervisorMonadConstraint m ⇒ NominalDiffTime → ContextMonad result worker_id m () -- {{{
updateInstataneousWorkloadStealTime (fromRational . toRational → current_value) = do
    current_time ← view current_time
    previous_value ← instantaneous_workload_steal_time %%= ((^.current_average_value) &&& addPointToExponentiallyWeightedAverage current_value current_time)
    current_value ← use (instantaneous_workload_steal_time . current_average_value)
    updateTimeWeightedStatisticsUsingLens instantaneous_workload_steal_time_statistics ((current_value + previous_value) / 2)
-- }}}

updateTimeWeightedStatistics :: (MonadIO m, Real α) ⇒ α → UTCTime → StateT (TimeWeightedStatistics α) m () -- {{{
updateTimeWeightedStatistics value current_time = do 
    last_time ← previous_time <<.= current_time
    last_value ← previous_value <<.= value
    let weight = fromRational . toRational $ (current_time `diffUTCTime` last_time)
        last_value_as_double = fromRational . toRational $ last_value
    first_moment += weight*last_value_as_double
    second_moment += weight*last_value_as_double*last_value_as_double
    minimum_value %= min value
    maximum_value %= max value
-- }}}

updateTimeWeightedStatisticsUsingLens :: -- {{{
    ∀ α m result worker_id. 
    (Real α, SupervisorMonadConstraint m) ⇒
    Lens' (SupervisorState result worker_id) (TimeWeightedStatistics α) →
    α →
    ContextMonad result worker_id m ()
updateTimeWeightedStatisticsUsingLens field value =
    view current_time >>= ContextMonad . void . zoom field . updateTimeWeightedStatistics value
-- }}}

validateWorkerKnown :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    String →
    worker_id →
    ContextMonad result worker_id m ()
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
    ContextMonad result worker_id m ()
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
    ContextMonad result worker_id m ()
validateWorkerNotKnown action worker_id = do
    Set.member worker_id <$> (use known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown action worker_id)
-- }}}

-- }}}
