-- @+leo-ver=5-thin
-- @+node:gcross.20111026172030.1274: * @file Control/Monad/Trans/Visitor/Worker/Reactive.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111026172030.1275: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Worker.Reactive where

-- @+<< Import needed modules >>
-- @+node:gcross.20111026172030.1276: ** << Import needed modules >>
import Control.Arrow ((&&&),second)
import Control.Exception (Exception(..),SomeException)
import Control.Concurrent (killThread)
import Control.Monad ((>=>),unless)
import Control.Monad.IO.Class

import Data.Either.Unwrap (fromLeft,fromRight,isLeft,isRight)
import Data.IORef (IORef,atomicModifyIORef)
import Data.Maybe (fromJust,isJust)
import Data.Monoid (mappend,mconcat)
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
-- @+node:gcross.20111026213013.1280: *3* VisitorWorkerReactiveRequest
data VisitorWorkerReactiveRequest id =
    StatusUpdateReactiveRequest
  | WorkloadStealReactiveRequest id
-- @+node:gcross.20111026172030.1281: ** Functions
-- @+node:gcross.20111026213013.1281: *3* addHandler
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
        IO (VisitorWorkerEnvironment α)
    ) →
    Event () →
    Event id →
    Event () →
    Event id →
    Event (Maybe VisitorWorkload) →
    VisitorT m α →
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (id,Maybe VisitorWorkload)
        ,Event ()
        ,Event id
        ,Event SomeException
        )

genericCreateVisitorTWorkerReactiveNetwork
    fork
    request_status_update_event
    request_workload_steal_event
    shutdown_event
    received_response_to_workload_request_event
    received_response_to_steal_request_event
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
                (fmap (const StatusUpdateReactiveRequest) request_status_update_event)
                (fmap WorkloadStealReactiveRequest request_workload_steal_event)
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
                    WorkloadStealReactiveRequest id → Right id
            ))
            $
            request_rejected_event

        (workload_not_received_event,workload_received_event) =
            switch . fmap (maybe (Left ()) Right) $ received_response_to_steal_request_event

        (redundant_workload_received_event,non_redundant_workload_received_event) =
            switchApply
                (fmap (\maybe_environment workload →
                    case maybe_environment of
                        Nothing → Right workload
                        Just _ → Left ()
                ) current_worker_environment)
                workload_received_event

        current_worker_environment_change_event =
            mconcat
                [fmap (const Nothing) worker_terminated_event
                ,fmap (const Nothing) shutdown_event
                ,fmap Just new_worker_environment_event
                ]

        worker_terminated_successfully_event =
            filterJust
            .
            fmap (\reason → case reason of
                VisitorWorkerFinished solutions → Just solutions
                _ → Nothing
            )
            $
            worker_terminated_event

        send_request_workload_event =
            mappend
                (fmap (const ()) worker_terminated_successfully_event)
                (fmap (const ()) workload_not_received_event)

        send_steal_workload_event = received_response_to_workload_request_event

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
            mconcat
                [update_maybe_status_event_
                ,(fmap (const Nothing) update_request_rejected_event)
                ,fmap
                    (\(Just (VisitorWorkerEnvironment{workerInitialPath})) solutions →
                        Just $
                        VisitorWorkerStatusUpdate
                            solutions
                            (VisitorWorkload workerInitialPath Explored)
                    )
                    current_worker_environment
                  <@>
                  worker_terminated_successfully_event
                 ]
        submit_maybe_workload_event =
            mappend
                submit_maybe_workload_event_
                (fmap (,Nothing) steal_request_rejected_event)

        failure_event :: Event SomeException
        failure_event =
            mappend
                (fmap (const . toException $ RedundantWorkloadReceived) redundant_workload_received_event)
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
                            WorkloadStealReactiveRequest id →
                                WorkloadStealRequested (submitMaybeWorkload . (id,))
                    ))
                )
            >>=
            flip unless (requestNotReceived request)
        )
        $
        request_receivable_event

    reactimate
        .
        fmap (
            fork workerTerminated visitor
            >=>
            newWorkerEnvironment
        )
        $
        non_redundant_workload_received_event

    reactimate
        .
        fmap (killThread . workerThreadId)
        .
        filterJust
        .
        (current_worker_environment <@)
        $
        shutdown_event

    return
        (update_maybe_status_event
        ,submit_maybe_workload_event
        ,send_request_workload_event
        ,send_steal_workload_event
        ,failure_event
        )
-- @+node:gcross.20111026220221.1457: *3* createVisitorTWorkerNetwork
createVisitorTWorkerReactiveNetwork ::
    (Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    Event () →
    Event id →
    Event () →
    Event id →
    Event (Maybe VisitorWorkload) →
    VisitorT m α →
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (id,Maybe VisitorWorkload)
        ,Event ()
        ,Event id
        ,Event SomeException
        )
createVisitorTWorkerReactiveNetwork run =
    genericCreateVisitorTWorkerReactiveNetwork
        (forkVisitorTWorkerThread run)
-- @+node:gcross.20111026220221.1459: *3* createVisitorWorkerNetwork
createVisitorWorkerReactiveNetwork ::
    Event () →
    Event id →
    Event () →
    Event id →
    Event (Maybe VisitorWorkload) →
    Visitor α →
    NetworkDescription
        (Event (Maybe (VisitorWorkerStatusUpdate α))
        ,Event (id,Maybe VisitorWorkload)
        ,Event ()
        ,Event id
        ,Event SomeException
        )
createVisitorWorkerReactiveNetwork =
    genericCreateVisitorTWorkerReactiveNetwork
        forkVisitorWorkerThread
-- @+node:gcross.20111026213013.1282: *3* switch
switch :: Event (Either a b) → (Event a,Event b)
switch = (fmap fromLeft . filterE isLeft) &&& (fmap fromRight . filterE isRight)
-- @+node:gcross.20111026220221.1283: *3* switchApply
switchApply :: Behavior (a → Either b c) → Event a → (Event b,Event c)
switchApply behavior event = switch (apply behavior event)
-- @-others
-- @-leo
