-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module LogicGrowsOnTrees.Parallel.Common.Supervisor.Implementation -- {{{
    ( AbortMonad()
    , ContextMonad()
    , FunctionOfTimeStatistics(..)
    , RunStatistics(..)
    , SupervisorCallbacks(..)
    , SupervisorFullConstraint
    , SupervisorMonadConstraint
    , SupervisorOutcome(..)
    , SupervisorOutcomeFor(..)
    , SupervisorTerminationReason(..)
    , SupervisorTerminationReasonFor(..)
    , SupervisorWorkerIdConstraint
    , IndependentMeasurementsStatistics(..)
    , abortSupervisor
    , abortSupervisorWithReason
    , addWorker
    , changeSupervisorOccupiedStatus
    , current_time
    , getCurrentProgress
    , getCurrentStatistics
    , getNumberOfWorkers
    , killWorkloadBuffer
    , liftContextToAbort
    , liftUserToAbort
    , localWithinAbort
    , localWithinContext
    , number_of_calls
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
import Control.Exception (Exception(..),throw)
import Control.Lens ((&))
import Control.Lens.Getter ((^.),use,uses,view)
import Control.Lens.Setter ((.~),(+~),(.=),(%=),(+=))
import Control.Lens.Internal.Zoom (Zoomed)
import Control.Lens.Lens ((<%=),(<<%=),(<<.=),(%%=),(<<.=),Lens')
import Control.Lens.TH (makeLenses)
import Control.Lens.Zoom (Zoom(..))
import Control.Monad ((>=>),liftM,liftM2,mplus,unless,void,when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader (asks)
import Control.Monad.Tools (ifM,whenM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Abort (AbortT(..),abort,runAbortT,unwrapAbortT)
import Control.Monad.Trans.Abort.Instances.MTL ()
import Control.Monad.Trans.Reader (ReaderT,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalStateT,execState,execStateT,runStateT)

import Data.Composition ((.*))
import Data.Derive.Monoid
import Data.DeriveTH
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
import Data.Typeable (Typeable)
import qualified Data.Time.Clock as Clock
import Data.Time.Clock (NominalDiffTime,UTCTime,diffUTCTime)
import Data.Word (Word)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Text.Printf

import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Path

import LogicGrowsOnTrees.Parallel.Common.ExplorationMode
import LogicGrowsOnTrees.Parallel.Common.Worker
import LogicGrowsOnTrees.Workload
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

{-| This datatype represents statistics obtained by integrating a value that is
    a function of time --- i.e., that holds a single value at any given point in
    time.
 -}
data FunctionOfTimeStatistics α = FunctionOfTimeStatistics -- {{{
    {   timeCount :: !Word {-^ the number of points at which the function changed -}
    ,   timeAverage :: !Double {-^ the average value of the function over the time period -}
    ,   timeStdDev :: !Double {-^ the standard deviation of the function over the time period -}
    ,   timeMin :: !α {-^ the minimum value of the function over the time period -}
    ,   timeMax :: !α {-^ the maximum value of the function over the time period -}
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

{-| Statistics gathered about the run. -}
data RunStatistics = -- {{{
    RunStatistics
    {   runStartTime :: !UTCTime {-^ the start time of the run -}
    ,   runEndTime :: !UTCTime {-^ the end time of the run -}
    ,   runWallTime :: !NominalDiffTime {-^ the wall time of the run -}
    ,   runSupervisorOccupation :: !Float {-^ the fraction of the time the supervisor spent processing an event -}
    ,   runSupervisorMonadOccupation :: !Float {-^ the fraction of the time the supervisor spent processing an event while inside the 'SupervisorMonad' -}
    ,   runNumberOfCalls :: !Int {-^ the number of calls made to functions in "LogicGrowsOnTrees.Parallel.Common.Supervisor" -}
    ,   runAverageTimePerCall :: !Float {-^ the average number of time per call made to functions in "LogicGrowsOnTrees.Parallel.Common.Supervisor" -}
    ,   runWorkerOccupation :: !Float {-^ the fraction of the total time that workers were occupied -}
    ,   runWorkerWaitTimes :: !(FunctionOfTimeStatistics NominalDiffTime) {-^ statistics for how long it took for workers to obtain a workload -}
    ,   runStealWaitTimes :: !IndependentMeasurementsStatistics {-^ statistics for the time needed to steal a workload from a worker -}
    ,   runWaitingWorkerStatistics :: !(FunctionOfTimeStatistics Int) {-^ statistics for the number of workers waiting for a workload -}
    ,   runAvailableWorkloadStatistics :: !(FunctionOfTimeStatistics Int) {-^ statistics for the number of available workloads waiting for a worker -}
    ,   runInstantaneousWorkloadRequestRateStatistics :: !(FunctionOfTimeStatistics Float) {-^ statistics for the instantaneous rate at which workloads were requested (using an exponentially decaying sum) -}
    ,   runInstantaneousWorkloadStealTimeStatistics :: !(FunctionOfTimeStatistics Float) {-^ statistics for the instantaneous time needed for workloads to be stolen (using an exponentially decaying weighted average) -}
    ,   runBufferSizeStatistics :: !(FunctionOfTimeStatistics Int) {-^ statistics for the size of the workload buffer -}
    } deriving (Eq,Show)
-- }}}

{-| This datatype represents statistics obtained by collecting a number of
    independent measurements.
 -}
data IndependentMeasurementsStatistics = IndependentMeasurementsStatistics -- {{{
    {   statCount :: {-# UNPACK #-} !Int {-^ the number of measurements -}
    ,   statAverage :: {-# UNPACK #-} !Double {-^ the average value -}
    ,   statStdDev ::  {-# UNPACK #-} !Double {-^ the standard deviation -}
    ,   statMin :: {-# UNPACK #-} !Double {-^ the minimum measuremnt value -}
    ,   statMax :: {-# UNPACK #-} !Double {-^ the maximum measurement value -}
    } deriving (Eq,Show)
-- }}}

data IndependentMeasurements = IndependentMeasurements -- {{{
    {   timeDataMin :: {-# UNPACK #-} !Min
    ,   timeDataMax :: {-# UNPACK #-} !Max
    ,   timeDataVariance ::  {-# UNPACK #-} !Variance
    } deriving (Eq,Show)
$( derive makeMonoid ''IndependentMeasurements )
-- }}}

data FunctionOfTime α = FunctionOfTime -- {{{
    {   _number_of_samples :: !Word
    ,   _previous_value :: !α
    ,   _previous_time :: !UTCTime
    ,   _first_moment :: !Double
    ,   _second_moment :: !Double
    ,   _minimum_value :: !α
    ,   _maximum_value :: !α
    } deriving (Eq,Show)
$( makeLenses ''FunctionOfTime )
-- }}}

newtype InterpolatedFunctionOfTime α = InterpolatedFunctionOfTime { _interpolated_function_of_time :: FunctionOfTime α } -- {{{
$( makeLenses ''InterpolatedFunctionOfTime )
-- }}}

newtype StepFunctionOfTime α = StepFunctionOfTime { _step_function_of_time :: FunctionOfTime α } -- {{{
$( makeLenses ''StepFunctionOfTime )
-- }}}

-- }}}

{-| Supervisor callbacks provide the means by which the supervisor logic
    communicates to the adapter, usually in order to tell it what it wants to
    say to various workers.
 -}
data SupervisorCallbacks exploration_mode worker_id m = -- {{{
    SupervisorCallbacks
    {   {-| This callback is used by the supervisor to signal that a progress
            update request should be send to the given list of workers.
         -}
        broadcastProgressUpdateToWorkers :: [worker_id] → m ()
    ,   {-| This callback is used by the supervisor to signal that a workload
            steal request should be send to the given list of workers.
         -}
        broadcastWorkloadStealToWorkers :: [worker_id] → m ()
    ,   {-| This callback is used by the supervisor to signal that the global
            progress update that was earlier requested by the adapter has
            finished, with the current progress given as the argument.
         -}
        receiveCurrentProgress :: ProgressFor exploration_mode → m ()
    ,   {-| This callback is used by the supervisor to signal that a workload
            should be send to the indicated worker.
         -}
     sendWorkloadToWorker :: Workload → worker_id → m ()
    }
-- }}}

data SupervisorConstants exploration_mode worker_id m = SupervisorConstants -- {{{
    {   callbacks :: !(SupervisorCallbacks exploration_mode worker_id m)
    ,   _current_time :: UTCTime
    ,   _exploration_mode :: ExplorationMode exploration_mode
    }
$( makeLenses ''SupervisorConstants )
-- }}}

data WorkloadBufferSizeParameters = WorkloadBufferSizeParameters -- {{{
    {   _minimum_size :: {-# UNPACK #-} !Int
    ,   _scale_factor :: {-# UNPACK #-} !Int
    } deriving (Eq,Show)
$( makeLenses ''WorkloadBufferSizeParameters )
-- }}}

data SupervisorState exploration_mode worker_id = -- {{{
    SupervisorState
    {   _waiting_workers_or_available_workloads :: !(Either (Map worker_id (Maybe UTCTime)) (Set Workload))
    ,   _known_workers :: !(Set worker_id)
    ,   _active_workers :: !(Map worker_id Workload)
    ,   _available_workers_for_steal :: !(IntMap (Set worker_id))
    ,   _workers_pending_workload_steal :: !(Set worker_id)
    ,   _workers_pending_progress_update :: !(Set worker_id)
    ,   _current_progress :: !(ProgressFor exploration_mode)
    ,   _debug_mode :: !Bool
    ,   _supervisor_occupation_statistics :: !OccupationStatistics
    ,   _worker_occupation_statistics :: !(Map worker_id OccupationStatistics)
    ,   _retired_worker_occupation_statistics :: !(Map worker_id RetiredOccupationStatistics)
    ,   _worker_wait_time_statistics :: !(InterpolatedFunctionOfTime NominalDiffTime)
    ,   _steal_request_matcher_queue :: !(MultiSet UTCTime)
    ,   _steal_request_failures :: !Int
    ,   _workload_steal_time_statistics :: !IndependentMeasurements
    ,   _waiting_worker_count_statistics :: !(StepFunctionOfTime Int)
    ,   _available_workload_count_statistics :: !(StepFunctionOfTime Int)
    ,   _instantaneous_workload_request_rate :: !ExponentiallyDecayingSum
    ,   _instantaneous_workload_request_rate_statistics :: !(StepFunctionOfTime Float)
    ,   _instantaneous_workload_steal_time :: !ExponentiallyWeightedAverage
    ,   _instantaneous_workload_steal_time_statistics :: !(StepFunctionOfTime Float)
    ,   _time_spent_in_supervisor_monad :: !NominalDiffTime
    ,   _workload_buffer_size :: !Int
    ,   _workload_buffer_size_parameters :: !WorkloadBufferSizeParameters
    ,   _workload_buffer_size_statistics :: !(StepFunctionOfTime Int)
    ,   _number_of_calls :: !Int
    }
$( makeLenses ''SupervisorState )
-- }}}

{-| The reason why the supervisor terminated. -}
data SupervisorTerminationReason final_result progress worker_id = -- {{{
    {-| the supervisor aborted before finishing;  included is the current progress at the time it aborted -}
    SupervisorAborted progress
    {-| the supervisor completed exploring the tree;  included is the final result -}
  | SupervisorCompleted final_result
    {-| the supervisor failed to explore the tree;  included is the worker where the failure occured as well as the message and the current progress at the time of failure -}
  | SupervisorFailure progress worker_id String
  deriving (Eq,Show)
-- }}}
{-| A convenient type alias for the 'SupervisorTerminationReason' associated with a given exploration mode. -} 
type SupervisorTerminationReasonFor exploration_mode = SupervisorTerminationReason (FinalResultFor exploration_mode) (ProgressFor exploration_mode)

{-| This type has the outcome of running the supervisor, which includes the
    reason why it terminated, the statistics for the run, and the workers that
    were present when it finished.
 -}
data SupervisorOutcome final_result progress worker_id = -- {{{
    SupervisorOutcome
    {   supervisorTerminationReason :: SupervisorTerminationReason final_result progress worker_id
    ,   supervisorRunStatistics :: RunStatistics
    ,   supervisorRemainingWorkers :: [worker_id]
    } deriving (Eq,Show)
-- }}}
{-| A convenient type alias for the 'SupervisorOutcome' associated with a given exploration mode. -} 
type SupervisorOutcomeFor exploration_mode worker_id = SupervisorOutcome (FinalResultFor exploration_mode) (ProgressFor exploration_mode) worker_id 

type InsideContextMonad exploration_mode worker_id m = -- {{{
    StateT (SupervisorState exploration_mode worker_id) (
        ReaderT (SupervisorConstants exploration_mode worker_id m)
            m
    )
-- }}}
newtype ContextMonad exploration_mode worker_id m α = ContextMonad -- {{{
    { unwrapContextMonad :: InsideContextMonad exploration_mode worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

type InsideAbortMonad exploration_mode worker_id m = -- {{{
    AbortT
        (SupervisorOutcomeFor exploration_mode worker_id)
        (ContextMonad exploration_mode worker_id m)
-- }}}
newtype AbortMonad exploration_mode worker_id m α = AbortMonad -- {{{
    { unwrapAbortMonad :: InsideAbortMonad exploration_mode worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

-- }}}

-- Contraints {{{
type SupervisorReaderConstraint exploration_mode worker_id m m' = MonadReader (SupervisorConstants exploration_mode worker_id m) m'
type SupervisorStateConstraint exploration_mode worker_id m' = MonadState (SupervisorState exploration_mode worker_id) m'

{-| This is the constraint placed on the monad in which the supervisor is running. -}
type SupervisorMonadConstraint m = (Functor m, MonadIO m)
{-| This is the constraint placed on the types that can be used as worker ids. -}
type SupervisorWorkerIdConstraint worker_id = (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id)
{-| This is just a sum of the 'SupervisorMonadConstraint' and the 'SupervisorWorkerIdConstraint'. -}
type SupervisorFullConstraint worker_id m = (SupervisorWorkerIdConstraint worker_id,SupervisorMonadConstraint m)
-- }}}

-- Classes {{{
class SpecializationOfFunctionOfTime f α where -- {{{
    zoomFunctionOfTimeWithInterpolator ::
        Monad m ⇒
        ((α → α → α) → StateT (FunctionOfTime α) m β) →
        StateT (f α) m β
-- }}}
-- }}}

-- Instances {{{

type instance Zoomed (AbortMonad exploration_mode worker_id m) = Zoomed (InsideAbortMonad exploration_mode worker_id m)

type instance Zoomed (ContextMonad exploration_mode worker_id m) = Zoomed (InsideContextMonad exploration_mode worker_id m)

instance Monad m ⇒ MonadReader (SupervisorConstants exploration_mode worker_id m) (AbortMonad exploration_mode worker_id m) where -- {{{
    ask = AbortMonad ask
    local f = AbortMonad . local f . unwrapAbortMonad
-- }}}

instance Monad m ⇒ MonadReader (SupervisorConstants exploration_mode worker_id m) (ContextMonad exploration_mode worker_id m) where -- {{{
    ask = ContextMonad ask
    local f = ContextMonad . local f . unwrapContextMonad
-- }}}

instance Monad m ⇒ MonadState (SupervisorState exploration_mode worker_id) (AbortMonad exploration_mode worker_id m) where -- {{{
    get = AbortMonad get
    put = AbortMonad . put
-- }}}

instance Monad m ⇒ MonadState (SupervisorState exploration_mode worker_id) (ContextMonad exploration_mode worker_id m) where -- {{{
    get = ContextMonad get
    put = ContextMonad . put
-- }}}

instance Monoid RetiredOccupationStatistics where -- {{{
    mempty = RetiredOccupationStatistics 0 0
    mappend x y = x & (occupied_time +~ y^.occupied_time) & (total_time +~ y^.total_time)
-- }}}

instance StatMonoid IndependentMeasurements NominalDiffTime where -- {{{
    pappend t =
        IndependentMeasurements
            <$> (pappend t' . timeDataMin)
            <*> (pappend t' . timeDataMax)
            <*> (pappend t' . timeDataVariance)
      where t' = realToFrac t :: Double
-- }}}

instance CalcCount IndependentMeasurements where -- {{{
    calcCount = calcCount . timeDataVariance
-- }}}

instance CalcMean IndependentMeasurements where -- {{{
    calcMean = calcMean . timeDataVariance
-- }}}

instance CalcVariance IndependentMeasurements where -- {{{
    calcVariance = calcVariance . timeDataVariance
    calcVarianceUnbiased = calcVarianceUnbiased . timeDataVariance
-- }}}

instance Fractional α ⇒ SpecializationOfFunctionOfTime InterpolatedFunctionOfTime α where -- {{{
    zoomFunctionOfTimeWithInterpolator constructAction = zoom interpolated_function_of_time (constructAction $ ((/2) .* (+)))
-- }}}

instance SpecializationOfFunctionOfTime StepFunctionOfTime α where -- {{{
    zoomFunctionOfTimeWithInterpolator constructAction = zoom step_function_of_time (constructAction . curry $ fst)
-- }}}

-- }}}

-- Functions {{{

abortSupervisor :: SupervisorFullConstraint worker_id m ⇒ AbortMonad exploration_mode worker_id m α -- {{{
abortSupervisor = use current_progress >>= abortSupervisorWithReason . SupervisorAborted
-- }}}

abortSupervisorWithReason ::  -- {{{
    SupervisorFullConstraint worker_id m ⇒
    SupervisorTerminationReasonFor exploration_mode worker_id →
    AbortMonad exploration_mode worker_id m α
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
    let old_value_weight = exp . realToFrac $ (previous_time `diffUTCTime` current_time)
        new_value_weight = 1 - old_value_weight
    current_average_value %= (+ new_value_weight * current_value) . (* old_value_weight)
-- }}}

addWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
beginWorkerOccupied = flip changeWorkerOccupiedStatus True
-- }}}

changeOccupiedStatus :: -- {{{
    ( Monad m'
    , SupervisorReaderConstraint exploration_mode worker_id m m'
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

changeSupervisorOccupiedStatus :: SupervisorMonadConstraint m ⇒ Bool → ContextMonad exploration_mode worker_id m () -- {{{
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
    ContextMonad exploration_mode worker_id m ()
changeWorkerOccupiedStatus worker_id =
    changeOccupiedStatus
        (fromJust . Map.lookup worker_id <$> use worker_occupation_statistics)
        ((worker_occupation_statistics %=) . Map.insert worker_id)
-- }}}

checkWhetherMoreStealsAreNeeded :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ContextMonad exploration_mode worker_id m ()
checkWhetherMoreStealsAreNeeded = do
    (number_of_waiting_workers,number_of_available_workloads) ←
        either (Map.size &&& const 0) (const 0 &&& Set.size)
        <$>
        use waiting_workers_or_available_workloads
    number_of_pending_workload_steals ← Set.size <$> use workers_pending_workload_steal
    available_workers ← use available_workers_for_steal
    when (number_of_pending_workload_steals == 0
       && number_of_waiting_workers > 0
       && IntMap.null available_workers
      ) $ throw OutOfSourcesForNewWorkloads
    workload_buffer_size ← use workload_buffer_size
    let number_of_needed_steals =
         (0 + workload_buffer_size + number_of_waiting_workers
            - number_of_available_workloads - number_of_pending_workload_steals
         ) `max` 0
    debugM $
        printf "needed steals (%i) = buffer size (%i) + waiting workers (%i) - available workloads (%i) - pending steals (%i)"
            number_of_needed_steals
            workload_buffer_size
            number_of_waiting_workers
            number_of_available_workloads
            number_of_pending_workload_steals
    when (number_of_needed_steals > 0) $ do
        let findWorkers accum 0 available_workers = (accum,available_workers)
            findWorkers accum n available_workers =
                case IntMap.minViewWithKey available_workers of
                    Nothing → (accum,IntMap.empty)
                    Just ((depth,workers),deeper_workers) →
                        go accum n workers
                      where
                        go accum 0 workers =
                            (accum
                            ,if Set.null workers
                                then deeper_workers
                                else IntMap.insert depth workers deeper_workers
                            )
                        go accum n workers =
                            case Set.minView workers of
                                Nothing → findWorkers accum n deeper_workers
                                Just (worker_id,rest_workers) → go (worker_id:accum) (n-1) rest_workers
        workers_to_steal_from ← available_workers_for_steal %%= findWorkers [] number_of_needed_steals
        let number_of_workers_to_steal_from = length workers_to_steal_from
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
    ContextMonad exploration_mode worker_id m ()
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
computeExponentialDecayCoefficient = (exp . realToFrac) .* diffUTCTime
-- }}}

computeInstantaneousRateFromDecayingSum :: -- {{{
    ( Functor m'
    , SupervisorMonadConstraint m
    , SupervisorReaderConstraint exploration_mode worker_id m m'
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
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
dequeueWorkerForSteal worker_id =
    getWorkerDepth worker_id >>= \depth →
        available_workers_for_steal %=
            IntMap.update
                (\queue → let new_queue = Set.delete worker_id queue
                          in if Set.null new_queue then Nothing else Just new_queue
                )
                depth
-- }}}

endWorkerOccupied :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad exploration_mode worker_id m ()
endWorkerOccupied = flip changeWorkerOccupiedStatus False
-- }}}

enqueueWorkerForSteal :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
enqueueWorkload workload =
    use waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers →
            case Map.minViewWithKey waiting_workers of
                Just ((free_worker_id,maybe_time_started_waiting),remaining_workers) → do
                    waiting_workers_or_available_workloads .= Left remaining_workers
                    maybe (return ())
                          (timePassedSince >=> updateFunctionOfTimeUsingLens worker_wait_time_statistics)
                          maybe_time_started_waiting
                    sendWorkloadTo workload free_worker_id
                    updateFunctionOfTimeUsingLens waiting_worker_count_statistics (Map.size remaining_workers)
                Nothing → do
                    waiting_workers_or_available_workloads .= Right (Set.singleton workload)
                    updateFunctionOfTimeUsingLens available_workload_count_statistics 1
        Right available_workloads → do
            waiting_workers_or_available_workloads .= Right (Set.insert workload available_workloads)
            updateFunctionOfTimeUsingLens available_workload_count_statistics (Set.size available_workloads + 1)
    >>
    checkWhetherMoreStealsAreNeeded
-- }}}

extractFunctionOfTimeStatistics :: -- {{{
    ( Ord α
    , Real α
    , SupervisorMonadConstraint m
    , SupervisorMonadConstraint m'
    , SupervisorReaderConstraint exploration_mode worker_id m m'
    , SpecializationOfFunctionOfTime f α
    ) ⇒
    UTCTime →
    m' (f α) →
    m' (FunctionOfTimeStatistics α)
extractFunctionOfTimeStatistics start_time getWeightedStatistics = do
    getWeightedStatistics >>= (evalStateT $ do
        zoomFunctionOfTime $ do
            total_weight ← realToFrac . (`diffUTCTime` start_time) <$> use previous_time
            timeCount ← use number_of_samples
            timeAverage ← (/total_weight) <$> use first_moment
            timeStdDev ← sqrt . (subtract $ timeAverage*timeAverage) . (/total_weight) <$> use second_moment
            timeMin ← use minimum_value
            timeMax ← use maximum_value
            return $ FunctionOfTimeStatistics{..}
     )
-- }}}

extractFunctionOfTimeStatisticsWithFinalPoint :: -- {{{
    ( Ord α
    , Real α
    , SupervisorMonadConstraint m
    , SupervisorMonadConstraint m'
    , SupervisorReaderConstraint exploration_mode worker_id m m'
    , SpecializationOfFunctionOfTime f α
    ) ⇒
    UTCTime →
    m' α →
    m' (f α) →
    m' (FunctionOfTimeStatistics α)
extractFunctionOfTimeStatisticsWithFinalPoint start_time getFinalValue getWeightedStatistics = do
    end_time ← view current_time
    let total_weight = realToFrac (end_time `diffUTCTime` start_time)
    final_value ← getFinalValue
    getWeightedStatistics >>= (evalStateT $ do
        updateFunctionOfTime final_value end_time
        zoomFunctionOfTime $ do
            timeCount ← use number_of_samples
            timeAverage ← (/total_weight) <$> use first_moment
            timeStdDev ← sqrt . (\x → x-timeAverage*timeAverage) . (/total_weight) <$> use second_moment
            timeMin ← min final_value <$> use minimum_value
            timeMax ← max final_value <$> use maximum_value
            return $ FunctionOfTimeStatistics{..}
     )
-- }}}

extractIndependentMeasurementsStatistics :: IndependentMeasurements → IndependentMeasurementsStatistics -- {{{
extractIndependentMeasurementsStatistics =
    IndependentMeasurementsStatistics
        <$>  calcCount
        <*>  calcMean
        <*>  calcStddev
        <*> (calcMin . timeDataMin)
        <*> (calcMax . timeDataMax)
-- }}}

finishWithResult :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    FinalResultFor exploration_mode →
    InsideAbortMonad exploration_mode worker_id m α
finishWithResult final_value =
    SupervisorOutcome
        <$> (return $ SupervisorCompleted final_value)
        <*> (lift getCurrentStatistics)
        <*> (Set.toList <$> use known_workers)
     >>= abort
-- }}}

getCurrentCheckpoint :: -- {{{
    ( SupervisorMonadConstraint m'
    , SupervisorStateConstraint exploration_mode worker_id m'
    , SupervisorReaderConstraint exploration_mode worker_id m m'
    ) ⇒ m' Checkpoint
getCurrentCheckpoint =
    liftM2 checkpointFromIntermediateProgress
        (view exploration_mode)
        (use current_progress)
-- }}}

getCurrentProgress :: SupervisorMonadConstraint m ⇒ ContextMonad exploration_mode worker_id m (ProgressFor exploration_mode) -- {{{
getCurrentProgress = use current_progress
-- }}}

getCurrentStatistics :: -- {{{
    ( SupervisorFullConstraint worker_id m
    , SupervisorMonadConstraint m'
    , SupervisorReaderConstraint exploration_mode worker_id m m'
    , SupervisorStateConstraint exploration_mode worker_id m'
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
        realToFrac
        .
        (/runWallTime)
        <$>
        use time_spent_in_supervisor_monad
    runNumberOfCalls ← use number_of_calls
    let runAverageTimePerCall = runSupervisorMonadOccupation / fromIntegral runNumberOfCalls
    runWorkerOccupation ←
        getOccupationFraction . mconcat . Map.elems
        <$>
        liftM2 (Map.unionWith (<>))
            (use retired_worker_occupation_statistics)
            (use worker_occupation_statistics >>= retireManyOccupationStatistics)
    runWorkerWaitTimes ←
        extractFunctionOfTimeStatistics
            runStartTime
            (use worker_wait_time_statistics)
    runStealWaitTimes ← extractIndependentMeasurementsStatistics <$> use workload_steal_time_statistics
    runWaitingWorkerStatistics ←
        extractFunctionOfTimeStatisticsWithFinalPoint
            runStartTime
            (either Map.size (const 0) <$> use waiting_workers_or_available_workloads)
            (use waiting_worker_count_statistics)
    runAvailableWorkloadStatistics ←
        extractFunctionOfTimeStatisticsWithFinalPoint
            runStartTime
            (either (const 0) Set.size <$> use waiting_workers_or_available_workloads)
            (use available_workload_count_statistics)
    runInstantaneousWorkloadRequestRateStatistics ←
        extractFunctionOfTimeStatisticsWithFinalPoint
            runStartTime
            (use instantaneous_workload_request_rate >>= computeInstantaneousRateFromDecayingSum)
            (use instantaneous_workload_request_rate_statistics)
    runInstantaneousWorkloadStealTimeStatistics ←
        extractFunctionOfTimeStatisticsWithFinalPoint
            runStartTime
            (use $ instantaneous_workload_steal_time . current_average_value)
            (use instantaneous_workload_steal_time_statistics)
    runBufferSizeStatistics ←
        extractFunctionOfTimeStatisticsWithFinalPoint
            runStartTime
            (use workload_buffer_size)
            (use workload_buffer_size_statistics)
    return RunStatistics{..}
-- }}}

getNumberOfWorkers :: SupervisorMonadConstraint m ⇒ ContextMonad exploration_mode worker_id m Int -- {{{
getNumberOfWorkers = liftM Set.size . use $ known_workers
-- }}}

getOccupationFraction :: RetiredOccupationStatistics → Float -- {{{
getOccupationFraction = realToFrac . liftA2 (/) (^.occupied_time) (^.total_time)
-- }}}

getWorkerDepth :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ContextMonad exploration_mode worker_id m Int
getWorkerDepth worker_id =
    maybe
        (error $ "Attempted to use the depth of inactive worker " ++ show worker_id ++ ".")
        workloadDepth
    .
    Map.lookup worker_id
    <$>
    use active_workers
-- }}}

initialFunctionForStartingTimeAndValue :: Num α ⇒ UTCTime → α → FunctionOfTime α -- {{{
initialFunctionForStartingTimeAndValue starting_time starting_value =
    FunctionOfTime
        0
        starting_value
        starting_time
        0
        0
        (fromIntegral (maxBound :: Int))
        (fromIntegral (minBound :: Int))
-- }}}

initialInterpolatedFunctionForStartingTime :: Num α ⇒ UTCTime → InterpolatedFunctionOfTime α -- {{{
initialInterpolatedFunctionForStartingTime = flip initialInterpolatedFunctionForStartingTimeAndValue 0
-- }}}

initialInterpolatedFunctionForStartingTimeAndValue :: Num α ⇒ UTCTime → α → InterpolatedFunctionOfTime α -- {{{
initialInterpolatedFunctionForStartingTimeAndValue = InterpolatedFunctionOfTime .* initialFunctionForStartingTimeAndValue
-- }}}

initialStepFunctionForStartingTime :: Num α ⇒ UTCTime → StepFunctionOfTime α -- {{{
initialStepFunctionForStartingTime = flip initialStepFunctionForStartingTimeAndValue 0
-- }}}

initialStepFunctionForStartingTimeAndValue :: Num α ⇒ UTCTime → α → StepFunctionOfTime α -- {{{
initialStepFunctionForStartingTimeAndValue = StepFunctionOfTime .* initialFunctionForStartingTimeAndValue
-- }}}

killWorkloadBuffer :: SupervisorMonadConstraint m ⇒ ContextMonad exploration_mode worker_id m () -- {{{
killWorkloadBuffer = do
    workload_buffer_size .= 0
    workload_buffer_size_parameters .= WorkloadBufferSizeParameters 0 0
-- }}}

liftContextToAbort :: Monad m ⇒ ContextMonad exploration_mode worker_id m α → AbortMonad exploration_mode worker_id m α -- {{{
liftContextToAbort = AbortMonad . lift
-- }}}

liftUserToAbort :: Monad m ⇒ m α → AbortMonad exploration_mode worker_id m α -- {{{
liftUserToAbort = liftContextToAbort . liftUserToContext
-- }}}

liftUserToContext :: Monad m ⇒ m α → ContextMonad exploration_mode worker_id m α -- {{{
liftUserToContext = ContextMonad . lift . lift
-- }}}

localWithinAbort :: -- {{{
    MonadReader e m ⇒
    (e → e) →
    AbortMonad exploration_mode worker_id m α →
    AbortMonad exploration_mode worker_id m α
localWithinAbort f = AbortMonad . AbortT . localWithinContext f . unwrapAbortT . unwrapAbortMonad
-- }}}

localWithinContext :: -- {{{
    MonadReader e m ⇒
    (e → e) →
    ContextMonad exploration_mode worker_id m α →
    ContextMonad exploration_mode worker_id m α
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
    ContextMonad exploration_mode worker_id m ()
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
    , SupervisorStateConstraint exploration_mode worker_id m'
    , SupervisorReaderConstraint exploration_mode worker_id m m'
    ) ⇒
    String →
    m' α →
    m' α
postValidate label action = action >>= \result →
  (use debug_mode >>= flip when (do
    checkpoint ← getCurrentCheckpoint
    debugM $ " === BEGIN VALIDATE === " ++ label
    use known_workers >>= debugM . ("Known workers is now " ++) . show
    use active_workers >>= debugM . ("Active workers is now " ++) . show
    use waiting_workers_or_available_workloads >>= debugM . ("Waiting/Available queue is now " ++) . show
    debugM . ("Current checkpoint is now " ++) . show $ checkpoint
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
    let total_workspace =
            mappend checkpoint
            .
            mconcat
            .
            map (flip checkpointFromInitialPath Explored . workloadPath . snd)
            $
            workers_and_workloads
    unless (total_workspace == Explored) $ throw $ IncompleteWorkspace total_workspace
    when (checkpoint == Explored) $
        if null workers_and_workloads
            then throw $ SpaceFullyExploredButSearchNotTerminated
            else throw $ SpaceFullyExploredButWorkloadsRemain workers_and_workloads
    debugM $ " === END VALIDATE === " ++ label
  )) >> return result
-- }}}

receiveProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdateFor exploration_mode →
    AbortMonad exploration_mode worker_id m ()
receiveProgressUpdate worker_id (ProgressUpdate progress_update remaining_workload) = AbortMonad . postValidate ("receiveProgressUpdate " ++ show worker_id ++ " ...") $ do
    infoM $ "Received progress update from " ++ show worker_id
    lift $ validateWorkerKnownAndActive "receiving progress update" worker_id
    updateCurrentProgress progress_update
    lift $ do
        is_pending_workload_steal ← Set.member worker_id <$> use workers_pending_workload_steal
        unless is_pending_workload_steal $ dequeueWorkerForSteal worker_id
        active_workers %= Map.insert worker_id remaining_workload
        unless is_pending_workload_steal $ enqueueWorkerForSteal worker_id
        clearPendingProgressUpdate worker_id
-- }}}

receiveStolenWorkload :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Maybe (StolenWorkloadFor exploration_mode) →
    AbortMonad exploration_mode worker_id m ()
receiveStolenWorkload worker_id maybe_stolen_workload = AbortMonad . postValidate ("receiveStolenWorkload " ++ show worker_id ++ " ...") $ do
    infoM $ "Received stolen workload from " ++ show worker_id
    lift $ validateWorkerKnownAndActive "receiving stolen workload" worker_id
    workers_pending_workload_steal %= Set.delete worker_id
    case maybe_stolen_workload of
        Nothing → steal_request_failures += 1
        Just (StolenWorkload (ProgressUpdate progress_update remaining_workload) workload) → do
            lift $
                (steal_request_matcher_queue %%= fromMaybe (error "Unable to find a request matching this steal!") . MultiSet.minView)
                  >>= (timePassedSince >=> liftA2 (>>) ((workload_steal_time_statistics %=) . pappend) updateInstataneousWorkloadStealTime)
            updateCurrentProgress progress_update
            lift $ do
                active_workers %= Map.insert worker_id remaining_workload
                enqueueWorkload workload
    lift $ do
        enqueueWorkerForSteal worker_id
        checkWhetherMoreStealsAreNeeded
-- }}}

receiveWorkerFinishedWithRemovalFlag :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    WorkerFinalProgressFor exploration_mode →
    AbortMonad exploration_mode worker_id m ()
receiveWorkerFinishedWithRemovalFlag remove_worker worker_id final_progress = AbortMonad . postValidate ("receiveWorkerFinished " ++ show worker_id) $ do
    infoM $ if remove_worker
        then "Worker " ++ show worker_id ++ " finished and removed."
        else "Worker " ++ show worker_id ++ " finished."
    lift $ validateWorkerKnownAndActive "the worker was declared finished" worker_id
    when remove_worker . lift $ retireWorker worker_id
    exploration_mode ← view exploration_mode
    (checkpoint,final_value) ←
        case exploration_mode of
            AllMode →
                (progressCheckpoint &&& progressResult)
                <$>
                updateCurrentProgress final_progress
            FirstMode → do
                let Progress{..} = final_progress
                checkpoint ← updateCurrentProgress progressCheckpoint
                case progressResult of
                    Nothing → return (checkpoint,Nothing)
                    Just solution → finishWithResult . Just $ Progress checkpoint solution
            FoundModeUsingPull f →
                (progressCheckpoint &&& Left . progressResult)
                <$>
                updateCurrentProgress final_progress
            FoundModeUsingPush _ → do
                (progressCheckpoint &&& Left . progressResult)
                <$>
                updateCurrentProgress final_progress
    case checkpoint of
        Explored → do
            active_worker_ids ← Map.keys . Map.delete worker_id <$> use active_workers
            unless (null active_worker_ids) . throw $
                ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
            finishWithResult final_value
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
    , SupervisorReaderConstraint exploration_mode worker_id m m'
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
    , SupervisorReaderConstraint exploration_mode worker_id m m'
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
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
retireAndDeactivateWorker worker_id = do
    retireWorker worker_id
    ifM (isJust . Map.lookup worker_id <$> use active_workers)
        (deactivateWorker True worker_id)
        (waiting_workers_or_available_workloads %%=
            either (pred . Map.size &&& Left . Map.delete worker_id)
                   (error $ "worker " ++ show worker_id ++ " is inactive but was not in the waiting queue")
         >>= updateFunctionOfTimeUsingLens waiting_worker_count_statistics
        )
-- }}}

runSupervisorStartingFrom :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    ProgressFor exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    (∀ α. AbortMonad exploration_mode worker_id m α) →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runSupervisorStartingFrom exploration_mode starting_progress callbacks program = liftIO Clock.getCurrentTime >>= \start_time →
    flip runReaderT (SupervisorConstants callbacks undefined exploration_mode)
    .
    flip evalStateT
        (SupervisorState
            {   _waiting_workers_or_available_workloads =
                    Right . Set.singleton $
                        Workload
                            Seq.empty
                            (checkpointFromIntermediateProgress
                                exploration_mode
                                starting_progress
                            )
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
            ,   _worker_wait_time_statistics = initialInterpolatedFunctionForStartingTime start_time
            ,   _steal_request_matcher_queue = mempty
            ,   _steal_request_failures = 0
            ,   _workload_steal_time_statistics = mempty
            ,   _waiting_worker_count_statistics = initialStepFunctionForStartingTime start_time
            ,   _available_workload_count_statistics = initialStepFunctionForStartingTime start_time
            ,   _instantaneous_workload_request_rate = ExponentiallyDecayingSum start_time 0
            ,   _instantaneous_workload_request_rate_statistics = initialStepFunctionForStartingTime start_time
            ,   _instantaneous_workload_steal_time = ExponentiallyWeightedAverage start_time 0
            ,   _instantaneous_workload_steal_time_statistics = initialStepFunctionForStartingTime start_time
            ,   _time_spent_in_supervisor_monad = 0
            ,   _workload_buffer_size = 4
            ,   _workload_buffer_size_parameters = WorkloadBufferSizeParameters 4 3
            ,   _workload_buffer_size_statistics = initialStepFunctionForStartingTimeAndValue start_time 4
            ,   _number_of_calls = 0
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
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
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

setSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ Bool → ContextMonad exploration_mode worker_id m () -- {{{
setSupervisorDebugMode = (debug_mode .=)
-- }}}

timePassedSince :: -- {{{
    ( Functor m'
    , SupervisorMonadConstraint m
    , SupervisorReaderConstraint exploration_mode worker_id m  m'
    ) ⇒
    UTCTime →
    m' NominalDiffTime
timePassedSince = (<$> view current_time) . flip diffUTCTime
-- }}}

tryGetWaitingWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ContextMonad exploration_mode worker_id m (Maybe worker_id)
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
    ContextMonad exploration_mode worker_id m ()
tryToObtainWorkloadFor is_new_worker worker_id =
    unless is_new_worker updateInstataneousWorkloadRequestRate
    >>
    use waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers → do
            maybe_time_started_waiting ← getMaybeTimeStartedWorking
            waiting_workers_or_available_workloads .= Left (Map.insert worker_id maybe_time_started_waiting waiting_workers)
            updateFunctionOfTimeUsingLens waiting_worker_count_statistics (Map.size waiting_workers + 1)
        Right available_workers →
            case Set.minView available_workers of
                Nothing → do
                    maybe_time_started_waiting ← getMaybeTimeStartedWorking
                    waiting_workers_or_available_workloads .= Left (Map.singleton worker_id maybe_time_started_waiting)
                    updateFunctionOfTimeUsingLens waiting_worker_count_statistics 1
                Just (workload,remaining_workloads) → do
                    unless is_new_worker $ updateFunctionOfTimeUsingLens worker_wait_time_statistics 0
                    sendWorkloadTo workload worker_id
                    waiting_workers_or_available_workloads .= Right remaining_workloads
                    updateFunctionOfTimeUsingLens available_workload_count_statistics (Set.size remaining_workloads + 1)
    >>
    checkWhetherMoreStealsAreNeeded
  where
    getMaybeTimeStartedWorking
      | is_new_worker = return Nothing
      | otherwise = Just <$> view current_time

-- }}}

updateBuffer :: -- {{{
    ( SupervisorMonadConstraint m'
    , SupervisorFullConstraint worker_id m
    ) ⇒ ContextMonad exploration_mode worker_id m ()
updateBuffer = do
    ratio ←
        liftM2 (*)
            (use instantaneous_workload_request_rate >>= computeInstantaneousRateFromDecayingSum)
            (use $ instantaneous_workload_steal_time . current_average_value)
    new_size ← uses workload_buffer_size_parameters $
        liftA2 max
            (^.minimum_size)
            (ceiling . (* ratio) . fromIntegral . (^.scale_factor))
    old_size ← workload_buffer_size <<.= new_size
    when (new_size /= old_size) $ do
        updateFunctionOfTimeUsingLens workload_buffer_size_statistics new_size
        when (new_size > old_size) checkWhetherMoreStealsAreNeeded
-- }}}

updateCurrentProgress :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ProgressFor exploration_mode →
    InsideAbortMonad exploration_mode worker_id m (ProgressFor exploration_mode)
updateCurrentProgress progress = do
    exploration_mode ← view exploration_mode
    case exploration_mode of
        AllMode → current_progress <%= (<> progress)
        FirstMode → current_progress <%= (<> progress)
        FoundModeUsingPull f → do
            progress@(Progress checkpoint result) ← current_progress <%= (<> progress)
            if f result
                then finishWithResult (Right progress)
                else return progress
        FoundModeUsingPush f → do
            progress@(Progress checkpoint result) ← current_progress <%= (<> progress)
            if f result
                then finishWithResult (Right progress)
                else return progress
-- }}}

updateFunctionOfTime :: -- {{{
    ( MonadIO m
    , Real α
    , SpecializationOfFunctionOfTime f α
    ) ⇒
    α →
    UTCTime →
    StateT (f α) m ()
updateFunctionOfTime value current_time =
  zoomFunctionOfTimeWithInterpolator $ \interpolate → do
    number_of_samples += 1
    last_time ← previous_time <<.= current_time
    last_value ← previous_value <<.= value
    let weight = realToFrac (current_time `diffUTCTime` last_time)
        interpolated_value = realToFrac $ interpolate last_value value
    first_moment += weight*interpolated_value
    second_moment += weight*interpolated_value*interpolated_value
    minimum_value %= min value
    maximum_value %= max value
-- }}}

updateFunctionOfTimeUsingLens :: -- {{{
    ( Real α
    , SupervisorMonadConstraint m
    , SpecializationOfFunctionOfTime f α
    ) ⇒
    Lens' (SupervisorState exploration_mode worker_id) (f α) →
    α →
    ContextMonad exploration_mode worker_id m ()
updateFunctionOfTimeUsingLens field value =
    view current_time >>= ContextMonad . void . zoom field . updateFunctionOfTime value
-- }}}

updateInstataneousWorkloadRequestRate :: SupervisorMonadConstraint m ⇒ ContextMonad exploration_mode worker_id m () -- {{{
updateInstataneousWorkloadRequestRate = do
    current_time ← view current_time
    previous_value ← instantaneous_workload_request_rate %%= ((^.decaying_sum_value) &&& addPointToExponentiallyDecayingSum current_time)
    current_value ← use (instantaneous_workload_request_rate . decaying_sum_value)
    updateFunctionOfTimeUsingLens instantaneous_workload_request_rate_statistics ((current_value + previous_value) / 2)
-- }}}

updateInstataneousWorkloadStealTime :: SupervisorMonadConstraint m ⇒ NominalDiffTime → ContextMonad exploration_mode worker_id m () -- {{{
updateInstataneousWorkloadStealTime (realToFrac → current_value) = do
    current_time ← view current_time
    previous_value ← instantaneous_workload_steal_time %%= ((^.current_average_value) &&& addPointToExponentiallyWeightedAverage current_value current_time)
    current_value ← use (instantaneous_workload_steal_time . current_average_value)
    updateFunctionOfTimeUsingLens instantaneous_workload_steal_time_statistics ((current_value + previous_value) / 2)
-- }}}

validateWorkerKnown :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    String →
    worker_id →
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
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
    ContextMonad exploration_mode worker_id m ()
validateWorkerNotKnown action worker_id = do
    Set.member worker_id <$> (use known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown action worker_id)
-- }}}

zoomFunctionOfTime :: -- {{{
    ( SpecializationOfFunctionOfTime f α
    , Monad m
    ) ⇒
    StateT (FunctionOfTime α) m β →
    StateT (f α) m β
zoomFunctionOfTime m = zoomFunctionOfTimeWithInterpolator (const m)
-- }}}

-- }}}
