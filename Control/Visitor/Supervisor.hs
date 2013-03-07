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
    , IndependentMeasurementsStatistics(..)
    , Statistics(..)
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
    , runSupervisorMaybeStartingFrom
    , runSupervisorStartingFrom
    , runUnrestrictedSupervisor
    , runUnrestrictedSupervisorMaybeStartingFrom
    , runUnrestrictedSupervisorStartingFrom
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

import Data.Time.Clock (diffUTCTime,getCurrentTime)
import Data.Composition ((.*),(.**))
import Data.Monoid (Monoid(mempty))

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor.Checkpoint (Progress)
import Control.Visitor.Worker (ProgressUpdate,StolenWorkload)

import qualified Control.Visitor.Supervisor.Implementation as Implementation
import Control.Visitor.Supervisor.Implementation -- {{{
    ( AbortMonad()
    , ContextMonad()
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , Statistics(..)
    , SupervisorCallbacks(..)
    , SupervisorFullConstraint
    , SupervisorMonadConstraint
    , SupervisorOutcome(..)
    , SupervisorTerminationReason(..)
    , SupervisorWorkerIdConstraint
    , current_time
    , liftContextToAbort
    , liftUserToAbort
    , localWithinContext
    , number_of_calls
    , time_spent_in_supervisor_monad
    ) -- }}}
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG]
-- }}}

-- Classes {{{
class WrappableIntoSupervisorMonad w where -- {{{
    wrapIntoSupervisorMonad :: MonadIO m ⇒ w result worker_id m α → SupervisorMonad result worker_id m α
-- }}}
-- }}}

-- Types {{{

newtype SupervisorMonad result worker_id m α = -- {{{
    SupervisorMonad {
        unwrapSupervisorMonad :: AbortMonad result worker_id m α
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

data SupervisorProgram result worker_id m = -- {{{
    ∀ α. BlockingProgram (SupervisorMonad result worker_id m ()) (m α) (α → SupervisorMonad result worker_id m ())
  | ∀ α. PollingProgram (SupervisorMonad result worker_id m ()) (m (Maybe α)) (α → SupervisorMonad result worker_id m ())
  | UnrestrictedProgram (∀ α. SupervisorMonad result worker_id m α)
-- }}}

-- }}}

-- Instances {{{

instance MonadTrans (SupervisorMonad result worker_id) where -- {{{
    lift = SupervisorMonad . liftUserToAbort
-- }}}

instance MonadReader r m ⇒ MonadReader r (SupervisorMonad result worker_id m) where -- {{{
    ask = lift ask
    local f = SupervisorMonad . Implementation.localWithinAbort f . unwrapSupervisorMonad
-- }}}

instance MonadState s m ⇒ MonadState s (SupervisorMonad result worker_id m) where -- {{{
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

abortSupervisor :: SupervisorFullConstraint worker_id m ⇒ SupervisorMonad result worker_id m α -- {{{
abortSupervisor = wrapIntoSupervisorMonad Implementation.abortSupervisor
-- }}}

addWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad result worker_id m ()
addWorker = wrapIntoSupervisorMonad . Implementation.addWorker
-- }}}

beginSupervisorOccupied :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m () -- {{{
beginSupervisorOccupied = changeSupervisorOccupiedStatus True
-- }}}

changeSupervisorOccupiedStatus :: SupervisorMonadConstraint m ⇒ Bool → SupervisorMonad result worker_id m () -- {{{
changeSupervisorOccupiedStatus = wrapIntoSupervisorMonad . Implementation.changeSupervisorOccupiedStatus
-- }}}

killWorkloadBuffer :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m () -- {{{
killWorkloadBuffer = wrapIntoSupervisorMonad Implementation.killWorkloadBuffer
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
getCurrentProgress = wrapIntoSupervisorMonad Implementation.getCurrentProgress
-- }}}

getCurrentStatistics :: -- {{{
    SupervisorFullConstraint worker_id m ⇒
    SupervisorMonad result worker_id m RunStatistics
getCurrentStatistics = SupervisorMonad Implementation.getCurrentStatistics
-- }}}

getNumberOfWorkers :: SupervisorMonadConstraint m ⇒ SupervisorMonad result worker_id m Int -- {{{
getNumberOfWorkers = wrapIntoSupervisorMonad Implementation.getNumberOfWorkers
-- }}}

performGlobalProgressUpdate :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad result worker_id m ()
performGlobalProgressUpdate = wrapIntoSupervisorMonad Implementation.performGlobalProgressUpdate
-- }}}

receiveProgressUpdate :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    ProgressUpdate result →
    SupervisorMonad result worker_id m ()
receiveProgressUpdate = wrapIntoSupervisorMonad .* Implementation.receiveProgressUpdate
-- }}}

receiveStolenWorkload :: -- {{{
    ( Monoid result
    , SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    Maybe (StolenWorkload result) →
    SupervisorMonad result worker_id m ()
receiveStolenWorkload = wrapIntoSupervisorMonad .* Implementation.receiveStolenWorkload
-- }}}

receiveWorkerFailure :: SupervisorFullConstraint worker_id m ⇒ worker_id → String → SupervisorMonad result worker_id m α -- {{{
receiveWorkerFailure =
    (wrapIntoSupervisorMonad . Implementation.abortSupervisorWithReason)
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
receiveWorkerFinishedWithRemovalFlag = wrapIntoSupervisorMonad .** Implementation.receiveWorkerFinishedWithRemovalFlag
-- }}}

removeWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad result worker_id m ()
removeWorker = wrapIntoSupervisorMonad . Implementation.removeWorker
-- }}}

removeWorkerIfPresent :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    worker_id →
    SupervisorMonad result worker_id m ()
removeWorkerIfPresent = wrapIntoSupervisorMonad . Implementation.removeWorkerIfPresent
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
runSupervisorStartingFrom starting_progress actions program =
    Implementation.runSupervisorStartingFrom
        starting_progress
        actions
        (unwrapSupervisorMonad . runSupervisorProgram $ program)
-- }}}

runSupervisorProgram :: SupervisorMonadConstraint m ⇒ SupervisorProgram result worker_id m → SupervisorMonad result worker_id m α -- {{{
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
setSupervisorDebugMode = wrapIntoSupervisorMonad . Implementation.setSupervisorDebugMode
-- }}}

tryGetWaitingWorker :: -- {{{
    ( SupervisorMonadConstraint m
    , SupervisorWorkerIdConstraint worker_id
    ) ⇒
    SupervisorMonad result worker_id m (Maybe worker_id)
tryGetWaitingWorker = wrapIntoSupervisorMonad Implementation.tryGetWaitingWorker
-- }}}

