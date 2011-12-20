-- @+leo-ver=5-thin
-- @+node:gcross.20111026172030.1274: * @file Control/Monad/Trans/Visitor/Reactive/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111026172030.1275: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Reactive.Worker where

-- @+<< Import needed modules >>
-- @+node:gcross.20111026172030.1276: ** << Import needed modules >>
import Control.Arrow ((&&&),second)
import Control.Exception (Exception(..),SomeException)
import Control.Concurrent (killThread)
import Control.Monad ((>=>),unless)
import Control.Monad.IO.Class
import Control.Monad.Tools (whenM,unlessM)

import Data.Derive.Monoid
import Data.DeriveTH
import Data.Either.Unwrap (fromLeft,fromRight,isLeft,isRight)
import Data.IORef (IORef,atomicModifyIORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.Maybe (fromJust,isJust)
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode ((⊕))
import Data.Sequence ((|>))

import Reactive.Banana

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111026220221.1284: ** Exceptions
-- @+node:gcross.20111026220221.1285: *3* RedundantWorkloadReceived
data RedundantWorkloadReceived = RedundantWorkloadReceived deriving (Eq,Show,Typeable)

instance Exception RedundantWorkloadReceived
-- @+node:gcross.20111026172030.1280: ** Types
-- @+node:gcross.20111219115425.1411: *3* WorkerIncomingEvents
data WorkerIncomingEvents = WorkerIncomingEvents
	{	workerIncomingStatusUpdateRequestedEvent :: Event ()
	,	workerIncomingWorkloadStealRequestedEvent :: Event ()
	,	workerIncomingShutdownEvent :: Event ()
	,	workerIncomingWorkloadReceivedEvent :: Event VisitorWorkload
	}

$( derive makeMonoid ''WorkerIncomingEvents )
-- @+node:gcross.20111219115425.1412: *3* WorkerOutgoingEvents
data WorkerOutgoingEvents α = WorkerOutgoingEvents
	{	workerOutgoingMaybeStatusUpdatedEvent :: Event (Maybe (VisitorWorkerStatusUpdate α))
	,	workerOutgoingMaybeWorkloadSubmittedEvent :: Event (Maybe (VisitorWorkerStatusUpdate α,VisitorWorkload))
	,	workerOutgoingFinishedEvent :: Event (VisitorWorkerFinalUpdate α)
	,	workerOutgoingFailureEvent :: Event SomeException
	}

$( derive makeMonoid ''WorkerOutgoingEvents )
-- @+node:gcross.20111026213013.1280: *3* VisitorWorkerReactiveRequest
data VisitorWorkerReactiveRequest =
    StatusUpdateReactiveRequest
  | WorkloadStealReactiveRequest
-- @+node:gcross.20111026172030.1281: ** Functions
-- @+node:gcross.20111219132352.1427: *3* (<$↔>)
infixl 4 <$↔>

(<$↔>) :: (a → Either b c) → Event a → (Event b,Event c)
f <$↔> x = split (f <$> x)
-- @+node:gcross.20111026220221.1283: *3* (<@↔>)
infixl 4 <@↔>

(<@↔>) :: Apply f Event ⇒ f (a → Either b c) → Event a → (Event b,Event c)
f <@↔> x = split (f <@> x)
-- @+node:gcross.20111117140347.1433: *3* createVisitorIOWorkerNetwork
createVisitorIOWorkerReactiveNetwork ::
    WorkerIncomingEvents →
    VisitorIO α →
    NetworkDescription (WorkerOutgoingEvents α)
createVisitorIOWorkerReactiveNetwork = createVisitorTWorkerReactiveNetwork id
-- @+node:gcross.20111026220221.1457: *3* createVisitorTWorkerNetwork
createVisitorTWorkerReactiveNetwork ::
    (Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    WorkerIncomingEvents →
    VisitorT m α →
    NetworkDescription (WorkerOutgoingEvents α)
createVisitorTWorkerReactiveNetwork run =
    genericCreateVisitorTWorkerReactiveNetwork
        (preforkVisitorTWorkerThread run)
-- @+node:gcross.20111026220221.1459: *3* createVisitorWorkerNetwork
createVisitorWorkerReactiveNetwork ::
    WorkerIncomingEvents →
    Visitor α →
    NetworkDescription (WorkerOutgoingEvents α)
createVisitorWorkerReactiveNetwork =
    genericCreateVisitorTWorkerReactiveNetwork
        preforkVisitorWorkerThread
-- @+node:gcross.20111026213013.1279: *3* filterJust
filterJust :: Event (Maybe a) → Event a
filterJust = fmap fromJust . filterE isJust
-- @+node:gcross.20111026172030.1277: *3* genericCreateVisitorTWorkerReactiveNetwork
genericCreateVisitorTWorkerReactiveNetwork ::
    (
        (VisitorWorkerTerminationReason α → IO ()) →
        VisitorT m α →
        VisitorWorkload →
        IO (IO (), VisitorWorkerEnvironment α)
    ) →
    WorkerIncomingEvents →
    VisitorT m α →
    NetworkDescription (WorkerOutgoingEvents α)

genericCreateVisitorTWorkerReactiveNetwork
    prefork
    WorkerIncomingEvents{..}
    visitor
    = do

    (request_not_received_event,requestNotReceived) ← newHandler
    (submit_maybe_workload_event,submitMaybeWorkload) ← newHandler
    (update_maybe_status_event,updateMaybeStatus) ← newHandler
    (worker_terminated_event,workerTerminated) ← newHandler
    (new_worker_environment_event,newWorkerEnvironment) ← newHandler

    let current_worker_environment = stepper Nothing current_worker_environment_change_event

        request_event =
            (StatusUpdateReactiveRequest <$ workerIncomingStatusUpdateRequestedEvent)
            ⊕
            (WorkloadStealReactiveRequest <$ workerIncomingWorkloadStealRequestedEvent)

        (request_not_receivable_event,request_receivable_event) =
            (\maybe_request_queue request →
                    case maybe_request_queue of
                        Nothing → Left request
                        Just request_queue → Right (workerPendingRequests request_queue,request)
            ) <$>  current_worker_environment
              <@↔> request_event

        request_rejected_event = request_not_received_event ⊕ request_not_receivable_event

        (update_request_rejected_event,steal_request_rejected_event) =
            (\request →
                case request of
                    StatusUpdateReactiveRequest → Left ()
                    WorkloadStealReactiveRequest → Right ()
            ) <$↔> request_rejected_event

        (redundant_workload_received_event,non_redundant_workload_received_event) =
            (\maybe_environment workload →
                    case maybe_environment of
                        Nothing → Right workload
                        Just _ → Left ()
            ) <$>  current_worker_environment
              <@↔> workerIncomingWorkloadReceivedEvent

        current_worker_environment_change_event =
            mconcat
                [Nothing <$ worker_terminated_event
                ,Nothing <$ workerIncomingShutdownEvent
                ,Just <$> new_worker_environment_event
                ]

        worker_terminated_successfully_event =
            filterJust
            $
            (\reason → case reason of
                VisitorWorkerFinished final_update → Just final_update
                _ → Nothing
            )
            <$>
            worker_terminated_event

        workerOutgoingFinishedEvent = worker_terminated_successfully_event

        worker_terminated_unsuccessfully_event =
            filterJust
            $
            (\reason → case reason of
                VisitorWorkerFailed exception → Just exception
                _ → Nothing
            )
            <$>
            worker_terminated_event

        workerOutgoingMaybeStatusUpdatedEvent =
            update_maybe_status_event
            ⊕
            (Nothing <$ update_request_rejected_event)

        workerOutgoingMaybeWorkloadSubmittedEvent =
            submit_maybe_workload_event
            ⊕
            (Nothing <$ steal_request_rejected_event)

        workerOutgoingFailureEvent :: Event SomeException
        workerOutgoingFailureEvent =
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
        workerIncomingShutdownEvent

    return WorkerOutgoingEvents{..}
-- @+node:gcross.20111026213013.1281: *3* newHandler
newHandler = do
    (event_handler,callback) ← liftIO newAddHandler
    event ← fromAddHandler event_handler
    return (event,callback)
-- @+node:gcross.20111026213013.1282: *3* split
split :: Event (Either a b) → (Event a,Event b)
split = (fmap fromLeft . filterE isLeft) &&& (fmap fromRight . filterE isRight)
-- @-others
-- @-leo
