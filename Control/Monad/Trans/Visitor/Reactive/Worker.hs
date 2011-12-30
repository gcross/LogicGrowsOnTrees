-- @+leo-ver=5-thin
-- @+node:gcross.20111026172030.1274: * @file Reactive/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111026172030.1275: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Reactive.Worker where

-- @+<< Import needed modules >>
-- @+node:gcross.20111026172030.1276: ** << Import needed modules >>
import Control.Arrow (second)
import Control.Exception (Exception(..),SomeException)
import Control.Concurrent (killThread)
import Control.Monad ((>=>),unless)
import Control.Monad.IO.Class
import Control.Monad.Tools (whenM,unlessM)

import Data.IORef (IORef,atomicModifyIORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode ((⊕))
import Data.Sequence ((|>))

import Reactive.Banana

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Reactive
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111026220221.1284: ** Exceptions
-- @+node:gcross.20111026220221.1285: *3* RedundantWorkloadReceived
data RedundantWorkloadReceived = RedundantWorkloadReceived deriving (Eq,Show,Typeable)

instance Exception RedundantWorkloadReceived
-- @+node:gcross.20111026172030.1280: ** Types
-- @+node:gcross.20111026213013.1280: *3* VisitorWorkerReactiveRequest
data VisitorWorkerReactiveRequest =
    StatusUpdateReactiveRequest
  | WorkloadStealReactiveRequest
-- @+node:gcross.20111219115425.1411: *3* VisitorWorkerIncomingEvents
data VisitorWorkerIncomingEvents = VisitorWorkerIncomingEvents
    {   visitorWorkerIncomingRequestEvent :: Event VisitorWorkerReactiveRequest
    ,   visitorWorkerIncomingShutdownEvent :: Event ()
    ,   visitorWorkerIncomingWorkloadReceivedEvent :: Event VisitorWorkload
    }
-- @+node:gcross.20111219115425.1412: *3* VisitorWorkerOutgoingEvents
data VisitorWorkerOutgoingEvents α = VisitorWorkerOutgoingEvents
    {   visitorWorkerOutgoingMaybeStatusUpdatedEvent :: Event (Maybe (VisitorWorkerStatusUpdate α))
    ,   visitorWorkerOutgoingMaybeWorkloadSubmittedEvent :: Event (Maybe (VisitorWorkerStolenWorkload α))
    ,   visitorWorkerOutgoingFinishedEvent :: Event (VisitorStatusUpdate α)
    ,   visitorWorkerOutgoingFailureEvent :: Event SomeException
    }
-- @+node:gcross.20111026172030.1281: ** Functions
-- @+node:gcross.20111117140347.1433: *3* createVisitorIOWorkerReactiveNetwork
createVisitorIOWorkerReactiveNetwork ::
    VisitorWorkerIncomingEvents →
    VisitorIO α →
    NetworkDescription (VisitorWorkerOutgoingEvents α)
createVisitorIOWorkerReactiveNetwork = createVisitorTWorkerReactiveNetwork id
-- @+node:gcross.20111026220221.1457: *3* createVisitorTWorkerReactiveNetwork
createVisitorTWorkerReactiveNetwork ::
    (Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    VisitorWorkerIncomingEvents →
    VisitorT m α →
    NetworkDescription (VisitorWorkerOutgoingEvents α)
createVisitorTWorkerReactiveNetwork run =
    genericCreateVisitorTWorkerReactiveNetwork
        (preforkVisitorTWorkerThread run)
-- @+node:gcross.20111026220221.1459: *3* createVisitorWorkerReactiveNetwork
createVisitorWorkerReactiveNetwork ::
    VisitorWorkerIncomingEvents →
    Visitor α →
    NetworkDescription (VisitorWorkerOutgoingEvents α)
createVisitorWorkerReactiveNetwork =
    genericCreateVisitorTWorkerReactiveNetwork
        preforkVisitorWorkerThread
-- @+node:gcross.20111026172030.1277: *3* genericCreateVisitorTWorkerReactiveNetwork
genericCreateVisitorTWorkerReactiveNetwork ::
    (
        (VisitorWorkerTerminationReason α → IO ()) →
        VisitorT m α →
        VisitorWorkload →
        IO (IO (), VisitorWorkerEnvironment α)
    ) →
    VisitorWorkerIncomingEvents →
    VisitorT m α →
    NetworkDescription (VisitorWorkerOutgoingEvents α)

genericCreateVisitorTWorkerReactiveNetwork
    prefork
    VisitorWorkerIncomingEvents{..}
    visitor
    = do

    (request_not_received_event,requestNotReceived) ← newHandler
    (submit_maybe_workload_event,submitMaybeWorkload) ← newHandler
    (update_maybe_status_event,updateMaybeStatus) ← newHandler
    (worker_terminated_event,workerTerminated) ← newHandler
    (new_worker_environment_event,newWorkerEnvironment) ← newHandler

    let current_worker_environment = stepper Nothing . mconcat $
            [Nothing <$ worker_terminated_event
            ,Nothing <$ visitorWorkerIncomingShutdownEvent
            ,Just <$> new_worker_environment_event
            ]

        (request_not_receivable_event,request_receivable_event) =
            (\maybe_request_queue request →
                    case maybe_request_queue of
                        Nothing → Left request
                        Just request_queue → Right (workerPendingRequests request_queue,request)
            ) <$>  current_worker_environment
              <@↔> visitorWorkerIncomingRequestEvent

        (update_request_rejected_event,steal_request_rejected_event) =
            (\request →
                case request of
                    StatusUpdateReactiveRequest → Left ()
                    WorkloadStealReactiveRequest → Right ()
            ) <$↔> (request_not_received_event ⊕ request_not_receivable_event)

        (redundant_workload_received_event,non_redundant_workload_received_event) =
            (\maybe_environment workload →
                    case maybe_environment of
                        Nothing → Right workload
                        Just _ → Left ()
            ) <$>  current_worker_environment
              <@↔> visitorWorkerIncomingWorkloadReceivedEvent

        (visitorWorkerOutgoingFinishedEvent,worker_terminated_unsuccessfully_event) =
            (\reason → case reason of
                VisitorWorkerFinished final_update → Just (Left final_update)
                VisitorWorkerFailed exception → Just (Right exception)
                VisitorWorkerAborted → Nothing
            ) <$?↔> worker_terminated_event

        visitorWorkerOutgoingMaybeStatusUpdatedEvent =
            update_maybe_status_event
            ⊕
            (Nothing <$ update_request_rejected_event)

        visitorWorkerOutgoingMaybeWorkloadSubmittedEvent =
            submit_maybe_workload_event
            ⊕
            (Nothing <$ steal_request_rejected_event)

        visitorWorkerOutgoingFailureEvent :: Event SomeException
        visitorWorkerOutgoingFailureEvent =
            (toException RedundantWorkloadReceived <$ redundant_workload_received_event)
            ⊕
            worker_terminated_unsuccessfully_event

    reactimate
        $
        (\(request_queue :: IORef (VisitorWorkerRequestQueue α),request) →
            atomicModifyIORef
                request_queue
                (maybe
                    (Nothing,False)
                    ((,True) . Just . (|>
                        case request of
                            StatusUpdateReactiveRequest →
                                StatusUpdateRequested updateMaybeStatus
                            WorkloadStealReactiveRequest →
                                WorkloadStealRequested submitMaybeWorkload
                    ))
                )
            >>=
            flip unless (requestNotReceived request)
        )
        <$>
        request_receivable_event

    reactimate
        $
        (\workload → do
            (start,worker_environment) ← prefork workerTerminated visitor workload
            newWorkerEnvironment worker_environment
            start
        )
        <$>
        non_redundant_workload_received_event

    reactimate
        .
        fmap (killThread . workerThreadId)
        .
        filterJust
        .
        (current_worker_environment <@)
        $
        visitorWorkerIncomingShutdownEvent

    return VisitorWorkerOutgoingEvents{..}
-- @-others
-- @-leo
