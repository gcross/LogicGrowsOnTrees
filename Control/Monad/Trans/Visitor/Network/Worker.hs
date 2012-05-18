-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Network.Worker where

-- Imports {{{
import Control.Concurrent (forkIO,killThread)
import Control.Concurrent.Chan (Chan,newChan,readChan,writeChan)
import Control.Concurrent.MVar (MVar,newEmptyMVar,putMVar,takeMVar)
import Control.Exception (Exception,SomeException,toException)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)

import Data.Monoid (Monoid)
import Data.Typeable

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Workload
import Control.Monad.Trans.Visitor.Worker
-- }}}

-- Exceptions {{{

data RedundantWorkloadReceived = RedundantWorkloadReceived deriving (Eq,Show,Typeable)

instance Exception RedundantWorkloadReceived

-- }}}

-- Types {{{

data VisitorWorkerReactiveRequest = -- {{{
    StatusUpdateReactiveRequest
  | WorkloadStealReactiveRequest
  deriving (Eq,Show)
-- }}}

data VisitorWorkerIncomingMessage = -- {{{
    VisitorWorkerIncomingStatusUpdateRequestMessage
  | VisitorWorkerIncomingWorkloadStealRequestMessage
  | VisitorWorkerIncomingShutdownMessage
  | VisitorWorkerIncomingWorkloadMessage VisitorWorkload
-- }}}

data VisitorWorkerInternalMessage α = -- {{{
    FailureMessage SomeException
  | FinishedMessage (VisitorStatusUpdate α)
-- }}}

data VisitorWorkerOutgoingMessage α = -- {{{
    VisitorWorkerOutgoingMaybeStatusUpdatedMessage (Maybe (VisitorWorkerStatusUpdate α))
  | VisitorWorkerOutgoingMaybeWorkloadSubmittedMessage (Maybe (VisitorWorkerStolenWorkload α))
  | VisitorWorkerOutgoingFinishedMessage (VisitorStatusUpdate α)
  | VisitorWorkerOutgoingFailureMessage SomeException
-- }}}

-- }}}

-- Functions {{{ 

createVisitorIOWorkerReactiveNetwork :: -- {{{
    Monoid α ⇒
    VisitorIO α →
    IO (Chan VisitorWorkerIncomingMessage, Chan (VisitorWorkerOutgoingMessage α))
createVisitorIOWorkerReactiveNetwork = createVisitorTWorkerReactiveNetwork id
-- }}}

createVisitorTWorkerReactiveNetwork :: -- {{{
    (Functor m, MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    VisitorT m α →
    IO (Chan VisitorWorkerIncomingMessage, Chan (VisitorWorkerOutgoingMessage α))
createVisitorTWorkerReactiveNetwork run =
    genericCreateVisitorTWorkerReactiveNetwork
        (preforkVisitorTWorkerThread run)
-- }}}

createVisitorWorkerReactiveNetwork :: -- {{{
    Monoid α ⇒
    Visitor α →
    IO (Chan VisitorWorkerIncomingMessage, Chan (VisitorWorkerOutgoingMessage α))
createVisitorWorkerReactiveNetwork =
    genericCreateVisitorTWorkerReactiveNetwork
        preforkVisitorWorkerThread
-- }}}

genericCreateVisitorTWorkerReactiveNetwork :: -- {{{
    Monoid α ⇒
    (
        (VisitorWorkerTerminationReason α → IO ()) →
        VisitorT m α →
        VisitorWorkload →
        IO (IO (), VisitorWorkerEnvironment α)
    ) →
    VisitorT m α →
    IO (Chan VisitorWorkerIncomingMessage, Chan (VisitorWorkerOutgoingMessage α))

genericCreateVisitorTWorkerReactiveNetwork prefork visitor = do
    incoming_internal_messages ← newChan
    incoming_external_messages ← newChan

    incoming_messages ← newChan
    outgoing_messages ← newChan

    forkIO $
        forever $
            readChan incoming_external_messages
            >>=
            return . Right
            >>=
            writeChan incoming_messages

    forkIO $
        forever $
            readChan incoming_internal_messages
            >>=
            return . Left
            >>=
            writeChan incoming_messages

    forkIO $
        let sendMessage = writeChan outgoing_messages

            handleInternalMessage (FinishedMessage status_update) = do
                sendMessage $ VisitorWorkerOutgoingFinishedMessage status_update
                loopWithoutEnvironment
            handleInternalMessage (FailureMessage exception) =
                sendMessage $ VisitorWorkerOutgoingFailureMessage exception

            handleExternalMessageWithoutEnvironment message =
                case message of
                    VisitorWorkerIncomingStatusUpdateRequestMessage → do
                        sendMessage $ VisitorWorkerOutgoingMaybeStatusUpdatedMessage Nothing
                        loopWithoutEnvironment
                    VisitorWorkerIncomingWorkloadStealRequestMessage → do
                        sendMessage $ VisitorWorkerOutgoingMaybeWorkloadSubmittedMessage Nothing
                        loopWithoutEnvironment
                    VisitorWorkerIncomingShutdownMessage → return ()
                    VisitorWorkerIncomingWorkloadMessage workload → do
                        (start,VisitorWorkerEnvironment{..}) ←
                            prefork
                                (\termination_reason → case termination_reason of
                                    VisitorWorkerFinished status_update →
                                        writeChan incoming_internal_messages (FinishedMessage status_update)
                                    VisitorWorkerFailed exception →
                                        writeChan incoming_internal_messages (FailureMessage exception)
                                    VisitorWorkerAborted → return ()
                                )
                                visitor
                                workload
                        forkIO start
                        loopWithEnvironment workerThreadId workerPendingRequests
    
            handleExternalMessageWithEnvironment thread_id request_queue message =
                case message of
                    VisitorWorkerIncomingStatusUpdateRequestMessage →
                        attemptSubmitToWorker
                            StatusUpdateRequested
                            VisitorWorkerOutgoingMaybeStatusUpdatedMessage
                            True
                    VisitorWorkerIncomingWorkloadStealRequestMessage →
                        attemptSubmitToWorker
                            WorkloadStealRequested
                            VisitorWorkerOutgoingMaybeWorkloadSubmittedMessage
                            False
                    VisitorWorkerIncomingShutdownMessage → abortWorker
                    VisitorWorkerIncomingWorkloadMessage _ → do
                        abortWorker
                        sendMessage $ VisitorWorkerOutgoingFailureMessage (toException RedundantWorkloadReceived)
              where
                abortWorker = killThread thread_id
    
                attemptSubmitToWorker buildRequest buildMessage failure_means_worker_has_stopped = do
                    response_mbox ← newEmptyMVar
                    successful ←
                        attemptAddToWorkerRequestQueue
                            request_queue
                            (buildRequest (putMVar response_mbox))
                    if successful
                        then do
                            response ← takeMVar response_mbox
                            submitMessage response
                            if failure_means_worker_has_stopped
                                then case response of
                                    Nothing → loopForWorkerStopped
                                    Just _ → loopForWorkerRunning
                                else
                                    loopForWorkerRunning
                        else do
                            submitMessage Nothing
                            loopForWorkerStopped
                  where
                    submitMessage = writeChan outgoing_messages . buildMessage
                    loopForWorkerRunning = loopWithEnvironment thread_id request_queue
                    loopForWorkerStopped = loopWithoutEnvironment
    
            loopWithoutEnvironment =
                readChan incoming_messages
                >>=
                either
                    handleInternalMessage
                    handleExternalMessageWithoutEnvironment
    
            loopWithEnvironment thread_id request_queue =
                readChan incoming_messages
                >>=
                either
                    handleInternalMessage
                    (handleExternalMessageWithEnvironment thread_id request_queue)

        in loopWithoutEnvironment

    return (incoming_external_messages,outgoing_messages)
-- }}}

-- }}}
