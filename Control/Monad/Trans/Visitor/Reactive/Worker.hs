-- @+leo-ver=5-thin
-- @+node:gcross.20111026172030.1274: * @file Control/Monad/Trans/Visitor/Reactive/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111026172030.1275: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
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
-- @+node:gcross.20111026213013.1280: *3* VisitorWorkerReactiveRequest
data VisitorWorkerReactiveRequest =
    StatusUpdateReactiveRequest
  | WorkloadStealReactiveRequest
-- @+node:gcross.20111026172030.1281: ** Functions
-- @+node:gcross.20111026213013.1281: *3* newHandler
newHandler = do
    (event_handler,callback) ← liftIO newAddHandler
    event ← fromAddHandler event_handler
    return (event,callback)
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
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (Maybe (VisitorWorkerStatusUpdate α,VisitorWorkload))
        ,Event (VisitorWorkerFinalUpdate α)
        ,Event SomeException
        )

genericCreateVisitorTWorkerReactiveNetwork
    prefork
    WorkerIncomingEvents{..}
    visitor
    = do

    (request_not_received_event,requestNotReceived) ← newHandler
    (submit_maybe_workload_event_,submitMaybeWorkload) ← newHandler
    (update_maybe_status_event_,updateMaybeStatus) ← newHandler
    (worker_terminated_event,workerTerminated) ← newHandler
    (new_worker_environment_event,newWorkerEnvironment) ← newHandler

    let current_worker_environment = stepper Nothing current_worker_environment_change_event

        request_event =
            mappend
                (fmap (const StatusUpdateReactiveRequest) workerIncomingStatusUpdateRequestedEvent)
                (fmap (const WorkloadStealReactiveRequest) workerIncomingWorkloadStealRequestedEvent)
        (request_not_receivable_event,request_receivable_event) =
            switchApply (
                fmap (\maybe_request_queue request →
                        case maybe_request_queue of
                            Nothing → Left request
                            Just request_queue → Right (workerPendingRequests request_queue,request)
                     )
                     current_worker_environment
            ) request_event

        request_rejected_event =
            mappend
                request_not_received_event
                request_not_receivable_event
        (update_request_rejected_event,steal_request_rejected_event) =
            switch
            .
            (fmap (\request →
                case request of
                    StatusUpdateReactiveRequest → Left ()
                    WorkloadStealReactiveRequest → Right ()
            ))
            $
            request_rejected_event

        (redundant_workerIncomingWorkloadReceivedEvent,non_redundant_workerIncomingWorkloadReceivedEvent) =
            switchApply
                (fmap (\maybe_environment workload →
                    case maybe_environment of
                        Nothing → Right workload
                        Just _ → Left ()
                ) current_worker_environment)
                workerIncomingWorkloadReceivedEvent

        current_worker_environment_change_event =
            mconcat
                [fmap (const Nothing) worker_terminated_event
                ,fmap (const Nothing) workerIncomingShutdownEvent
                ,fmap Just new_worker_environment_event
                ]

        worker_terminated_successfully_event =
            filterJust
            .
            fmap (\reason → case reason of
                VisitorWorkerFinished final_update → Just final_update
                _ → Nothing
            )
            $
            worker_terminated_event

        worker_finished_event = worker_terminated_successfully_event

        worker_terminated_unsuccessfully_event =
            filterJust
            .
            fmap (\reason → case reason of
                VisitorWorkerFailed exception → Just exception
                _ → Nothing
            )
            $
            worker_terminated_event

        update_maybe_status_event =
            mappend
                update_maybe_status_event_
                (fmap (const Nothing) update_request_rejected_event)
        submit_maybe_workload_event =
            mappend
                submit_maybe_workload_event_
                (fmap (const Nothing) steal_request_rejected_event)

        failure_event :: Event SomeException
        failure_event =
            mappend
                (fmap (const . toException $ RedundantWorkloadReceived) redundant_workerIncomingWorkloadReceivedEvent)
                worker_terminated_unsuccessfully_event

    reactimate
        .
        fmap (\(request_queue :: IORef (VisitorWorkerRequestQueue α),request) →
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
        $
        request_receivable_event

    reactimate
        .
        fmap (\workload → do
            (start,worker_environment) ← prefork workerTerminated visitor workload
            newWorkerEnvironment worker_environment
            start
        )
        $
        non_redundant_workerIncomingWorkloadReceivedEvent

    reactimate
        .
        fmap (killThread . workerThreadId)
        .
        filterJust
        .
        (current_worker_environment <@)
        $
        workerIncomingShutdownEvent

    return
        (update_maybe_status_event
        ,submit_maybe_workload_event
        ,worker_finished_event
        ,failure_event
        )
-- @+node:gcross.20111026220221.1457: *3* createVisitorTWorkerNetwork
createVisitorTWorkerReactiveNetwork ::
    (Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    WorkerIncomingEvents →
    VisitorT m α →
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (Maybe (VisitorWorkerStatusUpdate α,VisitorWorkload))
        ,Event (VisitorWorkerFinalUpdate α)
        ,Event SomeException
        )
createVisitorTWorkerReactiveNetwork run =
    genericCreateVisitorTWorkerReactiveNetwork
        (preforkVisitorTWorkerThread run)
-- @+node:gcross.20111117140347.1433: *3* createVisitorIOWorkerNetwork
createVisitorIOWorkerReactiveNetwork ::
    WorkerIncomingEvents →
    VisitorIO α →
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (Maybe (VisitorWorkerStatusUpdate α,VisitorWorkload))
        ,Event (VisitorWorkerFinalUpdate α)
        ,Event SomeException
        )
createVisitorIOWorkerReactiveNetwork = createVisitorTWorkerReactiveNetwork id
-- @+node:gcross.20111026220221.1459: *3* createVisitorWorkerNetwork
createVisitorWorkerReactiveNetwork ::
    WorkerIncomingEvents →
    Visitor α →
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (Maybe (VisitorWorkerStatusUpdate α,VisitorWorkload))
        ,Event (VisitorWorkerFinalUpdate α)
        ,Event SomeException
        )
createVisitorWorkerReactiveNetwork =
    genericCreateVisitorTWorkerReactiveNetwork
        preforkVisitorWorkerThread
-- @+node:gcross.20111026213013.1282: *3* switch
switch :: Event (Either a b) → (Event a,Event b)
switch = (fmap fromLeft . filterE isLeft) &&& (fmap fromRight . filterE isRight)
-- @+node:gcross.20111026220221.1283: *3* switchApply
switchApply :: Behavior (a → Either b c) → Event a → (Event b,Event c)
switchApply behavior event = switch (apply behavior event)
-- @-others
-- @-leo
