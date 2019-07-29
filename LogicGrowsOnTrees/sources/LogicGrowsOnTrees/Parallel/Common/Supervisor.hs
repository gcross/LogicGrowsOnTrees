{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- needed to define the MTL instances :-/
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-| The Supervisor module contains logic that is common to all of the adapters
    for the parallization infrastructure. The way to use it is to package the
    logic for communicating with your workers into a 'SupervisorProgram' that
    runs in the 'SupervisorMonad' with your state just below the
    'SupervisorMonad' in the monad stack.

    A great deal of the logic in this module deals with gathering statistics
    whose purpose is to provide data that can be used to figure out what is
    going wrong if the runtime is not scaling inversely with the number of
    workers.
 -}
module LogicGrowsOnTrees.Parallel.Common.Supervisor
    (
    -- * Types
    -- ** Statistics
      FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    -- ** Constraints
    , SupervisorFullConstraint
    , SupervisorWorkerIdConstraint
    -- ** Supervisor types
    , SupervisorCallbacks(..)
    , SupervisorMonad
    , SupervisorOutcome(..)
    , SupervisorOutcomeFor
    , SupervisorProgram(..)
    , SupervisorTerminationReason(..)
    , SupervisorTerminationReasonFor
    -- * Functions
    -- ** Worker interaction
    , addWorker
    , performGlobalProgressUpdate
    , receiveProgressUpdate
    , receiveStolenWorkload
    , receiveWorkerFailure
    , receiveWorkerFinished
    , receiveWorkerFinishedAndRemoved
    , receiveWorkerFinishedWithRemovalFlag
    , removeWorker
    , removeWorkerIfPresent
    -- ** Supervisor interaction
    , abortSupervisor
    , addWorkerCountListener
    , beginSupervisorOccupied
    , disableSupervisorDebugMode
    , enableSupervisorDebugMode
    , endSupervisorOccupied
    , setSupervisorDebugMode
    , setWorkloadBufferSize
    -- ** Inquiries
    , getCurrentProgress
    , getCurrentStatistics
    , getNumberOfWorkers
    , tryGetWaitingWorker
    -- ** Launching the supervisor
    , runSupervisor
    , runSupervisorStartingFrom
    -- ** Testing the supervisor
    , runTestSupervisor
    , runTestSupervisorStartingFrom
    ) where

import Prelude hiding (fail)

import Control.Applicative (Applicative)
import Control.Exception (AsyncException)
import Control.Lens.Setter ((.~),(+=))
import Control.Monad (forever)
import Control.Monad.Catch (MonadCatch(catch),MonadMask,MonadThrow)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (mapReaderT)
import Control.Monad.Trans.State.Strict (mapStateT)
import Control.Monad.Trans.Except (mapExceptT)

import Data.Time.Clock (diffUTCTime,getCurrentTime)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import LogicGrowsOnTrees.Parallel.Common.Worker (ProgressUpdateFor,StolenWorkloadFor)
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Path (WalkError(..))

import qualified LogicGrowsOnTrees.Parallel.Common.Supervisor.Implementation as Implementation
import LogicGrowsOnTrees.Parallel.Common.Supervisor.Implementation
    ( AbortMonad()
    , FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , SupervisorCallbacks(..)
    , SupervisorFullConstraint
    , SupervisorOutcome(..)
    , SupervisorOutcomeFor
    , SupervisorTerminationReason(..)
    , SupervisorTerminationReasonFor
    , SupervisorWorkerIdConstraint
    , current_time
    , number_of_calls
    , time_spent_in_supervisor_monad
    )

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG]

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

---------------------- Supervisor monad and program types ----------------------

{-| This is the monad in which the supervisor logic is run;  it keeps track of
    the state of the system including the current workers and their workloads,
    the current progress of the system, which workers we are waiting for a
    progress update or stolen workload from, etc.
 -}
newtype SupervisorMonad exploration_mode worker_id m α =
    SupervisorMonad {
        unwrapSupervisorMonad :: AbortMonad exploration_mode worker_id m α
    } deriving (Applicative,Functor,Monad,MonadCatch,MonadIO,MonadMask,MonadThrow)

instance MonadFail m ⇒ MonadFail (SupervisorMonad exploration_mode worker_id m) where
    fail = lift . fail

instance MonadTrans (SupervisorMonad exploration_mode worker_id) where
    lift = SupervisorMonad . lift . lift . lift

instance MonadReader e m ⇒ MonadReader e (SupervisorMonad exploration_mode worker_id m) where
    ask = lift ask
    local f =
        SupervisorMonad
        .
        mapExceptT (
            mapStateT (
                mapReaderT (local f)
            )
        )
        .
        unwrapSupervisorMonad

instance MonadState s m ⇒ MonadState s (SupervisorMonad exploration_mode worker_id m) where
    get = lift get
    put = lift . put


{-| A 'SupervisorProgram' is a specification of an event loop to be run inside
    the 'SupervisorMonad';  it exists in order to help the supervisor get an
    estimate for how much time it is spending doing work as opposed to waiting
    for a message from a worker so that it can generate accurate statistics
    about how much of the time it was occupied at the end of the run.
 -}
data SupervisorProgram exploration_mode worker_id m where
    {-| A 'BlockingProgram' has an event loop that executes an action that
        pauses the thread until an event occurs and then reacts to that event.
        The first argument is the supervisor action that initializes the system,
        the second argument is an action that blocks until an event has
        occurred, and the third argument is the supervisor action to run in
        response to the event.
     -}
    BlockingProgram :: ∀ exploration_mode worker_id m α.
        SupervisorMonad exploration_mode worker_id m () →
        m α →
        (α → SupervisorMonad exploration_mode worker_id m ()) →
        SupervisorProgram exploration_mode worker_id m
    {-| A 'PollingProgram' has an event loop that executes an action that
        checks whether an event has occurred and if so then reacts to that
        event. The first argument is the supervisor action that initializes the
        system, the second argument is an action that checks whether an event
        has occurred, and the third argument is the supervisor action to run in
        response to an event.
     -}
    PollingProgram :: ∀ exploration_mode worker_id m α.
        SupervisorMonad exploration_mode worker_id m () →
        m (Maybe α) →
        (α → SupervisorMonad exploration_mode worker_id m ()) →
        SupervisorProgram exploration_mode worker_id m
    {-| An 'TestProgram' is an event loop that you implement manually;
        note that it must run forever until the logic in the 'SupervisorMonad'
        This mode exists for testing rather than to be used by an adapter, but
        if you do use it then you take on responsibility for calling
        'beginSupervisorOccupied' and 'endSupervisorOccupied' when respectively
        the supervisor has begun and ended processing events so that the
        supervisor occupation statistics are correct.
     -}
    TestProgram :: ∀ exploration_mode worker_id m α.
        SupervisorMonad exploration_mode worker_id m α →
        SupervisorProgram exploration_mode worker_id m

------------------------ Wrapper convenience type-class ------------------------

wrapAbortIntoSupervisorMonad ∷
  MonadIO m ⇒
  AbortMonad exploration_mode worker_id m α →
  SupervisorMonad exploration_mode worker_id m α
wrapAbortIntoSupervisorMonad action = do
    time_at_entrance ← liftIO getCurrentTime
    result ← SupervisorMonad . local (current_time .~ time_at_entrance) $ do
        number_of_calls += 1
        debugM "Entering SupervisorMonad"
        result ← action
        debugM "Exiting SupervisorMonad"
        liftIO getCurrentTime >>= (time_spent_in_supervisor_monad +=) . (flip diffUTCTime time_at_entrance)
        return result
    return result
--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

------------------------------ Worker interaction ------------------------------

{-| Informs the supervisor that a worker has been added to the system;  the
    supervisor will attempt to obtain a workload for it, stealing one if
    necessary.
 -}
addWorker ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad exploration_mode worker_id m ()
addWorker = wrapAbortIntoSupervisorMonad . Implementation.addWorker

{-| Request that a global progress update be performed;  the supervisor will
    send progress update requests to all workers, and when it has received a
    response from everyone it will call the 'receiveCurrentProgress' callback in
    the 'SupervisorCallbacks'.
 -}
performGlobalProgressUpdate ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad exploration_mode worker_id m ()
performGlobalProgressUpdate = wrapAbortIntoSupervisorMonad Implementation.performGlobalProgressUpdate

{-| Informs the supervisor that a progress update has been received by a worker. -}
receiveProgressUpdate ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdateFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveProgressUpdate worker_id update =
    wrapAbortIntoSupervisorMonad $ Implementation.receiveProgressUpdate worker_id update

{-| Informs the supervisor that a worker has responded to a workload steal
    request;  a 'Nothing' indicates that the worker did not have a workload that
    could be stolen (which occurs if it hadn't taken any branches at the time
    the request was received).
 -}
receiveStolenWorkload ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Maybe (StolenWorkloadFor exploration_mode) →
    SupervisorMonad exploration_mode worker_id m ()
receiveStolenWorkload worker_id maybe_workload =
    wrapAbortIntoSupervisorMonad $ Implementation.receiveStolenWorkload worker_id maybe_workload

{-| Informs the supervisor that a worker has failed;  the system will be
    terminated and the given message returned as the failure message.
 -}
receiveWorkerFailure :: SupervisorFullConstraint worker_id m ⇒ worker_id → String → SupervisorMonad exploration_mode worker_id m α
receiveWorkerFailure worker_id message = do
    current_progress ← getCurrentProgress
    wrapAbortIntoSupervisorMonad
        .
        Implementation.abortSupervisorWithReason
        .
        SupervisorFailure current_progress worker_id
        $
        if message == show TreeEndedBeforeEndOfWalk ||
           message == show PastTreeIsInconsistentWithPresentTree
            then "The given checkpoint is not consistent with the given tree."
            else message

{-| Informs the supervisor that a worker has finished its current workload and
    returned the given final progress.
 -}
receiveWorkerFinished ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    WorkerFinishedProgressFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveWorkerFinished = receiveWorkerFinishedWithRemovalFlag False

{-| Informs the supervisor that a worker has finished its current workload and
    returned the given final progress; the worker will be removed after its
    final progress has been processed.
 -}
receiveWorkerFinishedAndRemoved ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    WorkerFinishedProgressFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveWorkerFinishedAndRemoved = receiveWorkerFinishedWithRemovalFlag True

{-| Informs the supervisor that a worker has finished its current workload and
    returned the given final progress;  if the first argument is 'True' then the
    worker will be removed.
 -}
receiveWorkerFinishedWithRemovalFlag ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    WorkerFinishedProgressFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveWorkerFinishedWithRemovalFlag remove_worker worker_id progress =
    wrapAbortIntoSupervisorMonad $
        Implementation.receiveWorkerFinishedWithRemovalFlag remove_worker worker_id progress

{-| Informs the supervisor that a worker (which might have been active and
    possibly even waited on for a progress update and/or stolen workload) has
    been removed; the worker will be removed from the set of workers with
    pending requests and its workload will be returned to the pool of available
    workloads.
 -}
removeWorker ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad exploration_mode worker_id m ()
removeWorker = wrapAbortIntoSupervisorMonad . Implementation.removeWorker

{-| Like 'removeWorker', but only acts if the worker is present. -}
removeWorkerIfPresent ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad exploration_mode worker_id m ()
removeWorkerIfPresent = wrapAbortIntoSupervisorMonad . Implementation.removeWorkerIfPresent

---------------------------- Supervisor interaction ----------------------------

{-| Aborts the supervisor. -}
abortSupervisor :: SupervisorFullConstraint worker_id m ⇒ SupervisorMonad exploration_mode worker_id m α
abortSupervisor = wrapAbortIntoSupervisorMonad Implementation.abortSupervisor

{-| Submits a function to be called whenever the number of workers changes; the
    given function will be also called immediately with the current number of
    workers.
 -}
addWorkerCountListener :: MonadIO m ⇒ (Int → IO ()) → SupervisorMonad exploration_mode worker_id m ()
addWorkerCountListener = wrapAbortIntoSupervisorMonad . Implementation.addWorkerCountListener

{-| Signals that the supervisor has begun processing an event. -}
beginSupervisorOccupied :: MonadIO m ⇒ SupervisorMonad exploration_mode worker_id m ()
beginSupervisorOccupied = changeSupervisorOccupiedStatus True

{-| Changes the occupied status of the supervisor. -}
changeSupervisorOccupiedStatus :: MonadIO m ⇒ Bool → SupervisorMonad exploration_mode worker_id m ()
changeSupervisorOccupiedStatus = wrapAbortIntoSupervisorMonad . Implementation.changeSupervisorOccupiedStatus

{-| Signals that the supervisor has finished processing an event. -}
endSupervisorOccupied :: MonadIO m ⇒ SupervisorMonad exploration_mode worker_id m ()
endSupervisorOccupied = changeSupervisorOccupiedStatus False

{-| Sets the workload buffer size, which is the minimum number of workloads that
    the supervisor will attempt to have available at all times so that requests
    for new workloads from workers can be responded to immediately.

    Normally the default value of 4 will be fine, but if you run into a problem
    where the amount of time needed to steal a workload is greater than the
    average time between requests for new workloads, then setting this to be
    proportional to the time needed to steal a workload divided by the time
    between workload requests may help.
 -}
setWorkloadBufferSize :: MonadIO m ⇒ Int → SupervisorMonad exploration_mode worker_id m ()
setWorkloadBufferSize = wrapAbortIntoSupervisorMonad . Implementation.setWorkloadBufferSize

---------------------------------- Inquiries -----------------------------------

{-| Gets the current progress of the system. -}
getCurrentProgress ::
    ( MonadIO m
    ) ⇒ SupervisorMonad exploration_mode worker_id m (ProgressFor exploration_mode)
getCurrentProgress = wrapAbortIntoSupervisorMonad Implementation.getCurrentProgress

{-| Gets the current statistics of the system. (Unlike the other \"get\"
    operations, there is a small but non-zero cost to do this as the statistics
    exist in an intermediate state that needs to be finalized.)
 -}
getCurrentStatistics ::
    SupervisorFullConstraint worker_id m ⇒
    SupervisorMonad exploration_mode worker_id m RunStatistics
getCurrentStatistics = wrapAbortIntoSupervisorMonad Implementation.getCurrentStatistics

{-| Gets the number of workers that are currently present in the system. -}
getNumberOfWorkers :: MonadIO m ⇒ SupervisorMonad exploration_mode worker_id m Int
getNumberOfWorkers = wrapAbortIntoSupervisorMonad Implementation.getNumberOfWorkers

{-| If there exists any workers waiting for a workload, it returns the id of one
    of them wrapped in 'Just'; it not, it returns 'Nothing'. (This is useful,
    for example, if you want to reduce the number of workers as it is best to
    start by removing ones that are currently idle.)
 -}
tryGetWaitingWorker ::
    ( MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad exploration_mode worker_id m (Maybe worker_id)
tryGetWaitingWorker = wrapAbortIntoSupervisorMonad Implementation.tryGetWaitingWorker

---------------------------------- Debugging -----------------------------------

{-| Turns off debug mode;  for more details see 'setSupervisorDebugMode'. -}
disableSupervisorDebugMode :: MonadIO m ⇒ SupervisorMonad exploration_mode worker_id m ()
disableSupervisorDebugMode = setSupervisorDebugMode False

{-| Turns on debug mode;  for more details see 'setSupervisorDebugMode'. -}
enableSupervisorDebugMode :: MonadIO m ⇒ SupervisorMonad exploration_mode worker_id m ()
enableSupervisorDebugMode = setSupervisorDebugMode True

{-| Sets whether the supervisor is in debug mode;  when it is in this mode it
    performs continuous self-consistency checks.  This mode is intended for
    assisting in debugging new adapters.
 -}
setSupervisorDebugMode :: MonadIO m ⇒ Bool → SupervisorMonad exploration_mode worker_id m ()
setSupervisorDebugMode = wrapAbortIntoSupervisorMonad . Implementation.setSupervisorDebugMode

--------------------------- Launching the supervisor ---------------------------

{-| Runs the supervisor in the given exploration mode with the given callbacks
    and program.
 -}
runSupervisor ::
    ( MonadCatch m
    , MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    SupervisorProgram exploration_mode worker_id m →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runSupervisor exploration_mode = runSupervisorStartingFrom exploration_mode (initialProgress exploration_mode)

{-| Like 'runSupervisor' but starting from the given progress. -}
runSupervisorStartingFrom ::
    ( MonadCatch m
    , MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    ProgressFor exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    SupervisorProgram exploration_mode worker_id m →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runSupervisorStartingFrom exploration_mode starting_progress callbacks program =
    Implementation.runSupervisorStartingFrom
        exploration_mode
        starting_progress
        callbacks
        (unwrapSupervisorMonad . runSupervisorProgram $ program)

{-| Converts a supervisor program into an infinite loop in the 'SupervisorMonad'. -}
runSupervisorProgram ::
    ( MonadCatch m
    , MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorProgram exploration_mode worker_id m →
    SupervisorMonad exploration_mode worker_id m α
runSupervisorProgram program =
    case program of
        BlockingProgram initialize getRequest processRequest → initialize >> forever (do
            debugM "Supervisor waiting for request."
            request ← lift getRequest
            debugM "Supervisor request has arrived; processing request..."
            beginSupervisorOccupied
            processRequest request
            endSupervisorOccupied
            debugM "...Supervisor finished processing request."
         )
        PollingProgram initialize getMaybeRequest processRequest → initialize >> forever (do
            maybe_request ← lift getMaybeRequest
            case maybe_request of
                Nothing → endSupervisorOccupied
                Just request → do
                    beginSupervisorOccupied
                    processRequest request
         )
        TestProgram run → run >> abortSupervisor
    `catch`
    (\(_ :: AsyncException) → abortSupervisor)

---------------------------- Testing the supervisor ----------------------------

{- $testing
The functions in this section are intended for testing purposes and normally
should not be used
 -}

{-| Runs the supervisor with a raw action in the 'SupervisorMonad'.

    NOTE:  You should not normally use this function, as it exists primarily for
           testing purposes;  see 'SupervisorProgram' for details.
 -}
runTestSupervisor ::
    ( MonadCatch m
    , MonadFail m
    , MonadIO m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    SupervisorMonad exploration_mode worker_id m α →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runTestSupervisor exploration_mode callbacks =
    runSupervisorStartingFrom exploration_mode (initialProgress exploration_mode) callbacks
    .
    TestProgram

{-| Like 'runTestSupervisor' but starting from the given progress. -}
runTestSupervisorStartingFrom ::
    ( MonadCatch m
    , MonadIO m
    , MonadFail m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    ProgressFor exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    SupervisorMonad exploration_mode worker_id m α →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runTestSupervisorStartingFrom exploration_mode starting_progress callbacks =
    runSupervisorStartingFrom exploration_mode starting_progress callbacks
    .
    TestProgram
