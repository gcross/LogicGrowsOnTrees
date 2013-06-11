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

module Visitor.Parallel.Common.Supervisor -- {{{
    ( FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , SupervisorCallbacks(..)
    , SupervisorFullConstraint
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
    , getCurrentStatistics
    , getNumberOfWorkers
    , killWorkloadBuffer
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
    , runSupervisorStartingFrom
    , runSupervisorInAllMode
    , runSupervisorInAllModeStartingFrom
    , runUnrestrictedSupervisor
    , runUnrestrictedSupervisorStartingFrom
    , runUnrestrictedSupervisorInAllMode
    , runUnrestrictedSupervisorInAllModeStartingFrom
    , setSupervisorDebugMode
    , tryGetWaitingWorker
    ) where -- }}}

-- Imports {{{
import Control.Applicative (Applicative)
import Control.Lens.Setter ((.~),(+=))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Monoid (Monoid)
import Data.Time.Clock (diffUTCTime,getCurrentTime)
import Data.Composition ((.*),(.**))

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import Visitor.Checkpoint (Progress)
import Visitor.Parallel.Common.Worker (ProgressUpdateFor,StolenWorkloadFor)
import qualified Visitor.Parallel.Common.Supervisor.Implementation as Implementation
import Visitor.Parallel.Common.Supervisor.Implementation -- {{{
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
    , SupervisorWorkerIdConstraint
    , current_time
    , liftContextToAbort
    , liftUserToAbort
    , number_of_calls
    , time_spent_in_supervisor_monad
    ) -- }}}
import Visitor.Parallel.Common.VisitorMode
import Visitor.Path (WalkError(..))
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG]
-- }}}

-- Classes {{{
class WrappableIntoSupervisorMonad w where -- {{{
    wrapIntoSupervisorMonad :: MonadIO m ⇒ w visitor_mode worker_id m α → SupervisorMonad visitor_mode worker_id m α
-- }}}
-- }}}

-- Types {{{

newtype SupervisorMonad visitor_mode worker_id m α = -- {{{
    SupervisorMonad {
        unwrapSupervisorMonad :: AbortMonad visitor_mode worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

data SupervisorProgram visitor_mode worker_id m = -- {{{
    ∀ α. BlockingProgram (SupervisorMonad visitor_mode worker_id m ()) (m α) (α → SupervisorMonad visitor_mode worker_id m ())
  | ∀ α. PollingProgram (SupervisorMonad visitor_mode worker_id m ()) (m (Maybe α)) (α → SupervisorMonad visitor_mode worker_id m ())
  | UnrestrictedProgram (∀ α. SupervisorMonad visitor_mode worker_id m α)
-- }}}

-- }}}

-- Instances {{{

instance MonadTrans (SupervisorMonad visitor_mode worker_id) where -- {{{
    lift = SupervisorMonad . liftUserToAbort
-- }}}

instance MonadReader e m ⇒ MonadReader e (SupervisorMonad visitor_mode worker_id m) where -- {{{
    ask = lift ask
    local f = SupervisorMonad . Implementation.localWithinAbort f . unwrapSupervisorMonad
-- }}}

instance MonadState s m ⇒ MonadState s (SupervisorMonad visitor_mode worker_id m) where -- {{{
    get = lift get
    put = lift . put
-- }}}

instance WrappableIntoSupervisorMonad AbortMonad where -- {{{
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
-- }}}

instance WrappableIntoSupervisorMonad ContextMonad where -- {{{
    wrapIntoSupervisorMonad = wrapIntoSupervisorMonad . liftContextToAbort
-- }}}

-- }}}

-- Exposed functions {{{

abortSupervisor :: SupervisorFullConstraint worker_id m ⇒ SupervisorMonad visitor_mode worker_id m α -- {{{
abortSupervisor = wrapIntoSupervisorMonad Implementation.abortSupervisor
-- }}}

addWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad visitor_mode worker_id m ()
addWorker = wrapIntoSupervisorMonad . Implementation.addWorker
-- }}}

beginSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad visitor_mode worker_id m () -- {{{
beginSupervisorOccupied = changeSupervisorOccupiedStatus True
-- }}}

changeSupervisorOccupiedStatus :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad visitor_mode worker_id m () -- {{{
changeSupervisorOccupiedStatus = wrapIntoSupervisorMonad . Implementation.changeSupervisorOccupiedStatus
-- }}}

killWorkloadBuffer :: SupervisorMonadConstraint m ⇒ SupervisorMonad visitor_mode worker_id m () -- {{{
killWorkloadBuffer = wrapIntoSupervisorMonad Implementation.killWorkloadBuffer
-- }}}

disableSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ SupervisorMonad visitor_mode worker_id m () -- {{{
disableSupervisorDebugMode = setSupervisorDebugMode False
-- }}}

enableSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ SupervisorMonad visitor_mode worker_id m () -- {{{
enableSupervisorDebugMode = setSupervisorDebugMode True
-- }}}

endSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad visitor_mode worker_id m () -- {{{
endSupervisorOccupied = changeSupervisorOccupiedStatus False
-- }}}

getCurrentProgress ::
    ( SupervisorMonadConstraint m
    ) ⇒ SupervisorMonad visitor_mode worker_id m (ProgressFor visitor_mode) -- {{{
getCurrentProgress = wrapIntoSupervisorMonad Implementation.getCurrentProgress
-- }}}

getCurrentStatistics :: -- {{{
    SupervisorFullConstraint worker_id m ⇒
    SupervisorMonad visitor_mode worker_id m RunStatistics
getCurrentStatistics = SupervisorMonad Implementation.getCurrentStatistics
-- }}}

getNumberOfWorkers :: SupervisorMonadConstraint m ⇒ SupervisorMonad visitor_mode worker_id m Int -- {{{
getNumberOfWorkers = wrapIntoSupervisorMonad Implementation.getNumberOfWorkers
-- }}}

performGlobalProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad visitor_mode worker_id m ()
performGlobalProgressUpdate = wrapIntoSupervisorMonad Implementation.performGlobalProgressUpdate
-- }}}

receiveProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdateFor visitor_mode →
    SupervisorMonad visitor_mode worker_id m ()
receiveProgressUpdate = wrapIntoSupervisorMonad .* Implementation.receiveProgressUpdate
-- }}}

receiveStolenWorkload :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Maybe (StolenWorkloadFor visitor_mode) →
    SupervisorMonad visitor_mode worker_id m ()
receiveStolenWorkload = wrapIntoSupervisorMonad .* Implementation.receiveStolenWorkload
-- }}}

receiveWorkerFailure :: SupervisorFullConstraint worker_id m ⇒ worker_id → String → SupervisorMonad visitor_mode worker_id m α -- {{{
receiveWorkerFailure worker_id message =
    wrapIntoSupervisorMonad
    .
    Implementation.abortSupervisorWithReason
    .
    SupervisorFailure worker_id
    $
    if message == show VisitorTerminatedBeforeEndOfWalk ||
       message == show PastVisitorIsInconsistentWithPresentVisitor
        then "The given checkpoint is not consistent with the given tree builder."
        else message
-- }}}

receiveWorkerFinished :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    WorkerFinalProgressFor visitor_mode →
    SupervisorMonad visitor_mode worker_id m ()
receiveWorkerFinished = receiveWorkerFinishedWithRemovalFlag False
-- }}}

receiveWorkerFinishedAndRemoved :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    WorkerFinalProgressFor visitor_mode →
    SupervisorMonad visitor_mode worker_id m ()
receiveWorkerFinishedAndRemoved = receiveWorkerFinishedWithRemovalFlag True
-- }}}

receiveWorkerFinishedWithRemovalFlag :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Bool →
    worker_id →
    WorkerFinalProgressFor visitor_mode →
    SupervisorMonad visitor_mode worker_id m ()
receiveWorkerFinishedWithRemovalFlag = wrapIntoSupervisorMonad .** Implementation.receiveWorkerFinishedWithRemovalFlag
-- }}}

removeWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad visitor_mode worker_id m ()
removeWorker = wrapIntoSupervisorMonad . Implementation.removeWorker
-- }}}

removeWorkerIfPresent :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad visitor_mode worker_id m ()
removeWorkerIfPresent = wrapIntoSupervisorMonad . Implementation.removeWorkerIfPresent
-- }}}

runSupervisor :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    VisitorMode visitor_mode →
    SupervisorCallbacks visitor_mode worker_id m →
    SupervisorProgram visitor_mode worker_id m →
    m (SupervisorOutcomeFor visitor_mode worker_id)
runSupervisor visitor_mode = runSupervisorStartingFrom visitor_mode (initialProgress visitor_mode)
-- }}}

runSupervisorStartingFrom :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    VisitorMode visitor_mode →
    ProgressFor visitor_mode →
    SupervisorCallbacks visitor_mode worker_id m →
    SupervisorProgram visitor_mode worker_id m →
    m (SupervisorOutcomeFor visitor_mode worker_id)
runSupervisorStartingFrom visitor_mode starting_progress callbacks program =
    Implementation.runSupervisorStartingFrom
        visitor_mode
        starting_progress
        callbacks
        (unwrapSupervisorMonad . runSupervisorProgram $ program)
-- }}}

runSupervisorInAllMode :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorCallbacks (AllMode result) worker_id m →
    SupervisorProgram (AllMode result) worker_id m →
    m (SupervisorOutcome result (Progress result) worker_id)
runSupervisorInAllMode = runSupervisor AllMode
-- }}}

runSupervisorInAllModeStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Progress result →
    SupervisorCallbacks (AllMode result) worker_id m →
    SupervisorProgram (AllMode result) worker_id m →
    m (SupervisorOutcome result (Progress result) worker_id)
runSupervisorInAllModeStartingFrom = runSupervisorStartingFrom AllMode
-- }}}

runSupervisorProgram :: -- {{{
    SupervisorMonadConstraint m ⇒
    SupervisorProgram visitor_mode worker_id m →
    SupervisorMonad visitor_mode worker_id m α
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
-- }}}

runUnrestrictedSupervisor :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    VisitorMode visitor_mode →
    SupervisorCallbacks visitor_mode worker_id m →
    (∀ α. SupervisorMonad visitor_mode worker_id m α) →
    m (SupervisorOutcomeFor visitor_mode worker_id)
runUnrestrictedSupervisor visitor_mode callbacks =
    runSupervisorStartingFrom visitor_mode (initialProgress visitor_mode) callbacks
    .
    UnrestrictedProgram
-- }}}

runUnrestrictedSupervisorStartingFrom :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    VisitorMode visitor_mode →
    ProgressFor visitor_mode →
    SupervisorCallbacks visitor_mode worker_id m →
    (∀ α. SupervisorMonad visitor_mode worker_id m α) →
    m (SupervisorOutcomeFor visitor_mode worker_id)
runUnrestrictedSupervisorStartingFrom visitor_mode starting_progress callbacks =
    runSupervisorStartingFrom visitor_mode starting_progress callbacks
    .
    UnrestrictedProgram
-- }}}

runUnrestrictedSupervisorInAllMode :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorCallbacks (AllMode result) worker_id m →
    (∀ α. SupervisorMonad (AllMode result) worker_id m α) →
    m (SupervisorOutcome result (Progress result) worker_id)
runUnrestrictedSupervisorInAllMode = runUnrestrictedSupervisor AllMode
-- }}}

runUnrestrictedSupervisorInAllModeStartingFrom :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    Progress result →
    SupervisorCallbacks (AllMode result) worker_id m →
    (∀ α. SupervisorMonad (AllMode result) worker_id m α) →
    m (SupervisorOutcome result (Progress result) worker_id)
runUnrestrictedSupervisorInAllModeStartingFrom = runUnrestrictedSupervisorStartingFrom AllMode 
-- }}}

setSupervisorDebugMode :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad visitor_mode worker_id m () -- {{{
setSupervisorDebugMode = wrapIntoSupervisorMonad . Implementation.setSupervisorDebugMode
-- }}}

tryGetWaitingWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad visitor_mode worker_id m (Maybe worker_id)
tryGetWaitingWorker = wrapIntoSupervisorMonad Implementation.tryGetWaitingWorker
-- }}}

