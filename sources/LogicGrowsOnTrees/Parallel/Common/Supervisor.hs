{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- needed to define the MTL instances :-/
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-| The Supervisor module contains logic that is common to all of the adapters
    for the parallization infrastructure. The way to use it is to build the
    logic for communicating with your workers into a 'SupervisorProgram' that
    runs in the 'SupervisorMonad' with your state just below the
    'SupervisorMonad' in the monad stack.

    A great deal of the logic in this module deals with gathering statistics.
    The main purpose of these statistics is twofold. First, these statistics
    provide data that can be used to determine why the running time is not
    scaling inversely linear to the number of processors. Second, the workload
    buffer (a set of workloads kept around so that worker does not have to wait
    to obtain a new workload) is designed to increase in size with the ratio of
    the time needed to steal a workload over the time between workload requests
    --- i.e., if the time needed to steal a workload is twice the time between
    workload requests then we want to send out twice as many workload steal
    requests so that the average wait time to steal a workload from someone is
    on the same order as the time between requests.
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
    , SupervisorMonadConstraint
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
    , runUnrestrictedSupervisor
    , runUnrestrictedSupervisorStartingFrom
    ) where


import Control.Applicative (Applicative)
import Control.Lens.Setter ((.~),(+=))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Time.Clock (diffUTCTime,getCurrentTime)
import Data.Composition ((.*),(.**))

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import LogicGrowsOnTrees.Parallel.Common.Worker (ProgressUpdateFor,StolenWorkloadFor)
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Path (WalkError(..))

import qualified LogicGrowsOnTrees.Parallel.Common.Supervisor.Implementation as Implementation
import LogicGrowsOnTrees.Parallel.Common.Supervisor.Implementation
    ( AbortMonad()
    , ContextMonad()
    , FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , SupervisorCallbacks(..)
    , SupervisorFullConstraint
    , SupervisorMonadConstraint
    , SupervisorOutcome(..)
    , SupervisorOutcomeFor
    , SupervisorTerminationReason(..)
    , SupervisorTerminationReasonFor
    , SupervisorWorkerIdConstraint
    , current_time
    , liftContextToAbort
    , liftUserToAbort
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

{-| This is the monad in which the supervisor logic should run;  it keeps track
    of the state of the system including the current workers and their
    workloads, the current progress of the system, which workers we are waiting
    for a progress update or stolen workload from, etc.
 -}
newtype SupervisorMonad exploration_mode worker_id m α =
    SupervisorMonad {
        unwrapSupervisorMonad :: AbortMonad exploration_mode worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)

instance MonadTrans (SupervisorMonad exploration_mode worker_id) where
    lift = SupervisorMonad . liftUserToAbort

instance MonadReader e m ⇒ MonadReader e (SupervisorMonad exploration_mode worker_id m) where
    ask = lift ask
    local f = SupervisorMonad . Implementation.localWithinAbort f . unwrapSupervisorMonad

instance MonadState s m ⇒ MonadState s (SupervisorMonad exploration_mode worker_id m) where
    get = lift get
    put = lift . put

{-| A 'SupervisorProgram' is a specification of an event loop to be run inside
    the 'SupervisorMonad';  it exists in order to help the supervisor get an
    estimate for how much time it is spending doing work as opposed to waiting
    for a message from a worker so that it can generate accurate statistics
    about how much of the time it was occupied at the end of the run.
 -}
data SupervisorProgram exploration_mode worker_id m =
    {-| A 'BlockingProgram' has an event loop that executes an action that
        pauses the thread until an event occurs and then reacts to that event.
        The first argument is the supervisor action that initializes the system,
        the second argument is an action that blocks until an event has
        occurred, and the third argument is the supervisor action to run in
        response to the event.
     -}
    ∀ α. BlockingProgram (SupervisorMonad exploration_mode worker_id m ()) (m α) (α → SupervisorMonad exploration_mode worker_id m ())
    {-| A 'PollingProgram' has an event loop that executes an action that
        checks whether an event has occurred and if so then reacts to that
        event. The first argument is the supervisor action that initializes the
        system, the second argument is an action that checks whether an event
        has occurred, and the third argument is the supervisor action to run in
        response to an event.
     -}
  | ∀ α. PollingProgram (SupervisorMonad exploration_mode worker_id m ()) (m (Maybe α)) (α → SupervisorMonad exploration_mode worker_id m ())
    {-| An 'UnrestrictedProgram' is an event loop that you implement manually;
        note that it must run forever until the logic in the 'SupervisorModule'
        decides to exit --- although you can always force it to abort by calling
        'abortSupervisor'.  This mode exists for testing rather than to be used
        by an adapter, but if you do use it then you take on responsibility for
        calling 'beginSupervisorOccupied' and 'endSupervisorOccupied' when
        respectively the supervisor has begun and ended processing an event so
        that the supervisor occupation statistics are correct.
     -}
  | UnrestrictedProgram (∀ α. SupervisorMonad exploration_mode worker_id m α)

------------------------ Wrapper convenience type-class ------------------------

class WrappableIntoSupervisorMonad w where
    wrapIntoSupervisorMonad :: MonadIO m ⇒ w exploration_mode worker_id m α → SupervisorMonad exploration_mode worker_id m α

instance WrappableIntoSupervisorMonad AbortMonad where
    wrapIntoSupervisorMonad action = do
        time_at_entrance ← liftIO getCurrentTime
        result ← SupervisorMonad . local (current_time .~ time_at_entrance) $ do
            number_of_calls += 1
            debugM "Entering SupervisorMonad"
            result ← action
            debugM "Exiting SupervisorMonad"
            liftIO getCurrentTime >>= (time_spent_in_supervisor_monad +=) . (flip diffUTCTime time_at_entrance)
            return result
        return result

instance WrappableIntoSupervisorMonad ContextMonad where
    wrapIntoSupervisorMonad = wrapIntoSupervisorMonad . liftContextToAbort

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

------------------------------ Worker interaction ------------------------------

{-| Informs the supervisor that a worker has been added to the system;  the
    supervisor will attempt to obtain a workload for it, stealing one if
    necessary.
 -}
addWorker ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad exploration_mode worker_id m ()
addWorker = wrapIntoSupervisorMonad . Implementation.addWorker

{-| Request that a global progress update be performed;  the supervisor will
    send progress update requests to all workers, and when it has received a
    response from everyone it will call the 'receiveCurrentProgress' callback in
    the 'SupervisorCallbacks'.
 -}
performGlobalProgressUpdate ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad exploration_mode worker_id m ()
performGlobalProgressUpdate = wrapIntoSupervisorMonad Implementation.performGlobalProgressUpdate

{-| Informs the supervisor that a progress update has been received by a worker. -}
receiveProgressUpdate ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdateFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveProgressUpdate = wrapIntoSupervisorMonad .* Implementation.receiveProgressUpdate

{-| Informs the supervisor that a worker has responded to a workload steal
    request;  a 'Nothing' indicates that the worker did not have a workload that
    could be stolen (which occurs if it hasn't taken any branches yet).
 -}
receiveStolenWorkload ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Maybe (StolenWorkloadFor exploration_mode) →
    SupervisorMonad exploration_mode worker_id m ()
receiveStolenWorkload = wrapIntoSupervisorMonad .* Implementation.receiveStolenWorkload

{-| Informs the supervisor that a worker has failed;  the system will be
    terminated and the given message returned as the failure message.
 -}
receiveWorkerFailure :: SupervisorFullConstraint worker_id m ⇒ worker_id → String → SupervisorMonad exploration_mode worker_id m α
receiveWorkerFailure worker_id message = do
    current_progress ← getCurrentProgress
    wrapIntoSupervisorMonad
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
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    WorkerFinalProgressFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveWorkerFinished = receiveWorkerFinishedWithRemovalFlag False

{-| Informs the supervisor that a worker has finished its current workload and
    returned the given final progress;  the worker should be removed after its
    final progress has been processed.
 -}
receiveWorkerFinishedAndRemoved ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    WorkerFinalProgressFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveWorkerFinishedAndRemoved = receiveWorkerFinishedWithRemovalFlag True

{-| Informs the supervisor that a worker has finished its current workload and
    returned the given final progress;  if the first argument is 'True' then the
    worker will be removed.
 -}
receiveWorkerFinishedWithRemovalFlag ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    WorkerFinalProgressFor exploration_mode →
    SupervisorMonad exploration_mode worker_id m ()
receiveWorkerFinishedWithRemovalFlag = wrapIntoSupervisorMonad .** Implementation.receiveWorkerFinishedWithRemovalFlag

{-| Informs the supervisor that a worker (which might have been active and
    possibly even being waited on for a progress update and/or stolen workload)
    has been removed;  its workload will be returned to the set of available
    workloads and it will be removed from the set of workers pending requests.
 -}
removeWorker ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad exploration_mode worker_id m ()
removeWorker = wrapIntoSupervisorMonad . Implementation.removeWorker

{-| Like 'removeWorker', but only acts if the worker is present. -}
removeWorkerIfPresent ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad exploration_mode worker_id m ()
removeWorkerIfPresent = wrapIntoSupervisorMonad . Implementation.removeWorkerIfPresent

---------------------------- Supervisor interaction ----------------------------

{-| Aborts the supervisor. -}
abortSupervisor :: SupervisorFullConstraint worker_id m ⇒ SupervisorMonad exploration_mode worker_id m α
abortSupervisor = wrapIntoSupervisorMonad Implementation.abortSupervisor

{-| Indicates that the supervisor has begun processing an event. -}
beginSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad exploration_mode worker_id m ()
beginSupervisorOccupied = changeSupervisorOccupiedStatus True

{-| Changes the occupied states of the supervisor. -}
changeSupervisorOccupiedStatus :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad exploration_mode worker_id m ()
changeSupervisorOccupiedStatus = wrapIntoSupervisorMonad . Implementation.changeSupervisorOccupiedStatus

{-| Indicates that the supervisor has finished processing an event. -}
endSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad exploration_mode worker_id m ()
endSupervisorOccupied = changeSupervisorOccupiedStatus False

{-| Sets the workload buffer size. -}
setWorkloadBufferSize :: SupervisorMonadConstraint m ⇒ Int → SupervisorMonad exploration_mode worker_id m ()
setWorkloadBufferSize = wrapIntoSupervisorMonad . Implementation.setWorkloadBufferSize

---------------------------------- Inquiries -----------------------------------

{-| Gets the current progress of the system. -}
getCurrentProgress ::
    ( SupervisorMonadConstraint m
    ) ⇒ SupervisorMonad exploration_mode worker_id m (ProgressFor exploration_mode)
getCurrentProgress = wrapIntoSupervisorMonad Implementation.getCurrentProgress

{-| Gets the current statistics of the system. -}
getCurrentStatistics ::
    SupervisorFullConstraint worker_id m ⇒
    SupervisorMonad exploration_mode worker_id m RunStatistics
getCurrentStatistics = SupervisorMonad Implementation.getCurrentStatistics

{-| Gets the number of workers that are present in the system. -}
getNumberOfWorkers :: SupervisorMonadConstraint m ⇒ SupervisorMonad exploration_mode worker_id m Int
getNumberOfWorkers = wrapIntoSupervisorMonad Implementation.getNumberOfWorkers

{-| If there exists any workers waiting for a workload, it returns the id of one
    of them wrapped in 'Just';  it not, it returns 'Nothing'.
 -}
tryGetWaitingWorker ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad exploration_mode worker_id m (Maybe worker_id)
tryGetWaitingWorker = wrapIntoSupervisorMonad Implementation.tryGetWaitingWorker

---------------------------------- Debugging -----------------------------------

{-| Turns off debug mode;  for more details see 'setSupervisorDebugMode'. -}
disableSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ SupervisorMonad exploration_mode worker_id m ()
disableSupervisorDebugMode = setSupervisorDebugMode False

{-| Turns on debug mode;  for more details see 'setSupervisorDebugMode'. -}
enableSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ SupervisorMonad exploration_mode worker_id m ()
enableSupervisorDebugMode = setSupervisorDebugMode True

{-| Sets whether the supervisor is in debug mode;  when it is in this mode it
    performs continuous self-consistency checks.  This mode is intended for
    assisting in debugging new adapters.
 -}
setSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad exploration_mode worker_id m ()
setSupervisorDebugMode = wrapIntoSupervisorMonad . Implementation.setSupervisorDebugMode

--------------------------- Launching the supervisor ---------------------------

{-| Runs the supervisor in the given exploration mode, with the given callbacks, and
    running the given program.
 -}
runSupervisor ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    SupervisorProgram exploration_mode worker_id m →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runSupervisor exploration_mode = runSupervisorStartingFrom exploration_mode (initialProgress exploration_mode)

{-| Runs the supervisor in the given exploration mode, with the given callbacks,
    running the given program, and starting from the given progress.
 -}
runSupervisorStartingFrom ::
    ( SupervisorMonadConstraint m
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
    SupervisorMonadConstraint m ⇒
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
        UnrestrictedProgram run → run

---------------------------- Testing the supervisor ----------------------------

{- $testing
The functions in this section are intended for testing purposes and normally
should not be used
 -}

{-| Runs the supervisor with a raw action in the 'SupervisorMonad'. -}
runUnrestrictedSupervisor ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    (∀ α. SupervisorMonad exploration_mode worker_id m α) →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runUnrestrictedSupervisor exploration_mode callbacks =
    runSupervisorStartingFrom exploration_mode (initialProgress exploration_mode) callbacks
    .
    UnrestrictedProgram

{-| Runs the supervisor with a raw action in the 'SupervisorMonad' that starts from the given progress. -}
runUnrestrictedSupervisorStartingFrom ::
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    ExplorationMode exploration_mode →
    ProgressFor exploration_mode →
    SupervisorCallbacks exploration_mode worker_id m →
    (∀ α. SupervisorMonad exploration_mode worker_id m α) →
    m (SupervisorOutcomeFor exploration_mode worker_id)
runUnrestrictedSupervisorStartingFrom exploration_mode starting_progress callbacks =
    runSupervisorStartingFrom exploration_mode starting_progress callbacks
    .
    UnrestrictedProgram
