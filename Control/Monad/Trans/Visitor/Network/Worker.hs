-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Network.Worker -- Exports {{{
    ( VisitorWorkerInbox
    , VisitorWorkerOutbox
    , VisitorWorkerOutgoingMessage(..)
    , createVisitorIONetworkWorker
    , createVisitorTNetworkWorker
    , createVisitorNetworkWorker
    , genericCreateVisitorTNetworkWorker
    , withVisitorIONetworkWorker
    , withVisitorTNetworkWorker
    , withVisitorNetworkWorker
    , genericWithVisitorTNetworkWorker
    , sendStatusUpdateRequestTo
    , sendWorkloadStealRequestTo
    , sendShutdownCommandTo
    , sendWorkloadTo
    , receiveTaggedMessageFrom
    , receiveMessageFrom
    ) where -- }}}

-- Imports {{{
import Control.Concurrent (forkIO,killThread)
import Control.Concurrent.Chan (Chan,newChan,readChan,writeChan)
import Control.Concurrent.MVar (MVar,newEmptyMVar,putMVar,takeMVar)
import Control.Exception (Exception,finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)

import Data.Functor ((<$>),fmap)
import Data.Monoid (Monoid)
import Data.Typeable

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Workload
import Control.Monad.Trans.Visitor.Worker
-- }}}

-- Types {{{

data VisitorWorkerIncomingMessage α = -- {{{
    VisitorWorkerIncomingStatusUpdateRequestMessage
  | VisitorWorkerIncomingWorkloadStealRequestMessage
  | VisitorWorkerIncomingShutdownMessage
  | VisitorWorkerIncomingWorkloadMessage VisitorWorkload
  | VisitorWorkerIncomingFinishedMessage (VisitorStatusUpdate α)
  | VisitorWorkerIncomingFailureMessage String
  deriving (Eq,Show)
-- }}}

newtype VisitorWorkerInbox α = VisitorWorkerInbox { incoming_messages :: Chan (VisitorWorkerIncomingMessage α) }

data VisitorWorkerOutgoingMessage α = -- {{{
    VisitorWorkerOutgoingMaybeStatusUpdatedMessage (Maybe (VisitorWorkerStatusUpdate α))
  | VisitorWorkerOutgoingMaybeWorkloadSubmittedMessage (Maybe (VisitorWorkerStolenWorkload α))
  | VisitorWorkerOutgoingFinishedMessage (VisitorStatusUpdate α)
  | VisitorWorkerOutgoingFailureMessage String
  deriving (Eq,Show)
-- }}}

newtype VisitorWorkerOutbox tag α = VisitorWorkerOutbox { outgoing_messages :: Chan (tag,VisitorWorkerOutgoingMessage α) }

-- }}}

-- Functions {{{ 

-- Direct worker creation {{{
createVisitorIONetworkWorker :: -- {{{
    Monoid α ⇒
    VisitorWorkerOutbox tag α →
    VisitorWorkerInbox α →
    tag →
    VisitorIO α →
    IO ()
createVisitorIONetworkWorker = createVisitorTNetworkWorker id
-- }}}
createVisitorTNetworkWorker :: -- {{{
    (Functor m, MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    VisitorWorkerOutbox tag α →
    VisitorWorkerInbox α →
    tag →
    VisitorT m α →
    IO ()
createVisitorTNetworkWorker run =
    genericCreateVisitorTNetworkWorker
        (preforkVisitorTWorkerThread run)
-- }}}
createVisitorNetworkWorker :: -- {{{
    Monoid α ⇒
    VisitorWorkerOutbox tag α →
    VisitorWorkerInbox α →
    tag →
    Visitor α →
    IO ()
createVisitorNetworkWorker =
    genericCreateVisitorTNetworkWorker
        preforkVisitorWorkerThread
-- }}}
genericCreateVisitorTNetworkWorker :: -- {{{
    Monoid α ⇒
    (
        (VisitorWorkerTerminationReason α → IO ()) →
        VisitorT m α →
        VisitorWorkload →
        IO (IO (), VisitorWorkerEnvironment α)
    ) →
    VisitorWorkerOutbox tag α →
    VisitorWorkerInbox α →
    tag →
    VisitorT m α →
    IO ()
genericCreateVisitorTNetworkWorker
    prefork
    VisitorWorkerOutbox{..}
    VisitorWorkerInbox{..}
    tag
    visitor
    = do

    forkIO $
        let sendMessage = writeChan outgoing_messages . (tag,)
            handleMessageWithoutEnvironment message =
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
                                        writeChan incoming_messages $
                                            (VisitorWorkerIncomingFinishedMessage status_update)
                                    VisitorWorkerFailed exception →
                                        writeChan incoming_messages $
                                            (VisitorWorkerIncomingFailureMessage . show $ exception)
                                    VisitorWorkerAborted → return ()
                                )
                                visitor
                                workload
                        forkIO start
                        loopWithEnvironment workerThreadId workerPendingRequests
                    VisitorWorkerIncomingFinishedMessage status_update → do
                        sendMessage $ VisitorWorkerOutgoingFinishedMessage status_update
                        loopWithoutEnvironment
                    VisitorWorkerIncomingFailureMessage exception → 
                        sendMessage $ VisitorWorkerOutgoingFailureMessage exception
    
            handleMessageWithEnvironment thread_id request_queue message =
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
                        sendMessage $ VisitorWorkerOutgoingFailureMessage "Redundant workload received."
                    VisitorWorkerIncomingFinishedMessage status_update → do
                        sendMessage $ VisitorWorkerOutgoingFinishedMessage status_update
                        loopWithoutEnvironment
                    VisitorWorkerIncomingFailureMessage exception → 
                        sendMessage $ VisitorWorkerOutgoingFailureMessage exception
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
                    submitMessage = writeChan outgoing_messages . (tag,) . buildMessage
                    loopForWorkerRunning = loopWithEnvironment thread_id request_queue
                    loopForWorkerStopped = loopWithoutEnvironment
    
            loopWithoutEnvironment =
                readChan incoming_messages
                >>=
                handleMessageWithoutEnvironment
    
            loopWithEnvironment thread_id request_queue =
                readChan incoming_messages
                >>=
                (handleMessageWithEnvironment thread_id request_queue)

        in loopWithoutEnvironment
    return ()
-- }}}
-- }}}

-- Indirect worker creation {{{
withVisitorIONetworkWorker :: -- {{{
    Monoid α ⇒
    VisitorIO α →
    (VisitorWorkerInbox α → VisitorWorkerOutbox () α → IO β) →
    IO β
withVisitorIONetworkWorker = withVisitorTNetworkWorker id
-- }}}
withVisitorTNetworkWorker :: -- {{{
    (Functor m, MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    VisitorT m α →
    (VisitorWorkerInbox α → VisitorWorkerOutbox () α → IO β) →
    IO β
withVisitorTNetworkWorker run =
    genericWithVisitorTNetworkWorker
        (preforkVisitorTWorkerThread run)
-- }}}
withVisitorNetworkWorker :: -- {{{
    Monoid α ⇒
    Visitor α →
    (VisitorWorkerInbox α → VisitorWorkerOutbox () α → IO β) →
    IO β
withVisitorNetworkWorker =
    genericWithVisitorTNetworkWorker
        preforkVisitorWorkerThread
-- }}}
genericWithVisitorTNetworkWorker :: -- {{{
    Monoid α ⇒
    (
        (VisitorWorkerTerminationReason α → IO ()) →
        VisitorT m α →
        VisitorWorkload →
        IO (IO (), VisitorWorkerEnvironment α)
    ) →
    VisitorT m α →
    (VisitorWorkerInbox α → VisitorWorkerOutbox () α → IO β) →
    IO β
genericWithVisitorTNetworkWorker prefork visitor run = do
    inbox ← VisitorWorkerInbox <$> newChan
    outbox ← VisitorWorkerOutbox <$> newChan
    flip finally (sendShutdownCommandTo inbox) $ do
        genericCreateVisitorTNetworkWorker prefork outbox inbox () visitor
        run inbox outbox
-- }}}
-- }}}

-- Worker interaction {{{
sendStatusUpdateRequestTo :: VisitorWorkerInbox α → IO () -- {{{
sendStatusUpdateRequestTo VisitorWorkerInbox{..} =
    writeChan incoming_messages VisitorWorkerIncomingStatusUpdateRequestMessage
-- }}}
sendWorkloadStealRequestTo :: VisitorWorkerInbox α → IO () -- {{{
sendWorkloadStealRequestTo VisitorWorkerInbox{..} =
    writeChan incoming_messages VisitorWorkerIncomingWorkloadStealRequestMessage
-- }}}
sendShutdownCommandTo :: VisitorWorkerInbox α → IO () -- {{{
sendShutdownCommandTo VisitorWorkerInbox{..} =
    writeChan incoming_messages VisitorWorkerIncomingShutdownMessage
-- }}}
sendWorkloadTo :: VisitorWorkload → VisitorWorkerInbox α → IO () -- {{{
sendWorkloadTo workload VisitorWorkerInbox{..} =
    writeChan incoming_messages (VisitorWorkerIncomingWorkloadMessage workload)
-- }}}
receiveTaggedMessageFrom :: VisitorWorkerOutbox tag α → IO (tag,VisitorWorkerOutgoingMessage α) -- {{{
receiveTaggedMessageFrom VisitorWorkerOutbox{..} = readChan outgoing_messages
-- }}}
receiveMessageFrom :: VisitorWorkerOutbox () α → IO (VisitorWorkerOutgoingMessage α) -- {{{
receiveMessageFrom = fmap snd . receiveTaggedMessageFrom
-- }}}
-- }}}

-- }}}
