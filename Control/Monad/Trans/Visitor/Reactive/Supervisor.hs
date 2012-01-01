-- @+leo-ver=5-thin
-- @+node:gcross.20111223170956.1430: * @file Reactive/Supervisor.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111219132352.1418: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Reactive.Supervisor where

-- @+<< Import needed modules >>
-- @+node:gcross.20111219132352.1419: ** << Import needed modules >>
import Control.Exception (Exception,throw)
import Control.Monad (void)

import qualified Data.Foldable as Fold
import Data.Functor
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq,ViewL(..),(|>))
import Data.Typeable (Typeable)

import Reactive.Banana.Incremental
import Reactive.Banana.Model

import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Reactive
import Control.Monad.Trans.Visitor.Reactive.Worker
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20120101200431.1880: ** Exceptions
-- @+node:gcross.20120101200431.1881: *3* IncompleteVisitError
data IncompleteVisitError = IncompleteVisitError VisitorCheckpoint deriving Typeable

instance Show IncompleteVisitError where
    show (IncompleteVisitError checkpoint) = "After visiting the entire search space, the cumulative checkpoint was not Explored but " ++ show checkpoint

instance Exception IncompleteVisitError
-- @+node:gcross.20111219132352.1420: ** Types
-- @+node:gcross.20111226153030.1436: *3* WorkerIdTagged
data WorkerIdTagged worker_id α = WorkerIdTagged
    {   workerId :: worker_id
    ,   workerIdTaggedData :: α
    } deriving (Eq,Show)
-- @+node:gcross.20111219132352.1421: *3* VisitorSupervisorIncomingEvents
data VisitorSupervisorIncomingEvents ξ worker_id α = VisitorSupervisorIncomingEvents
    {   visitorSupervisorIncomingWorkerAddedEvent :: Event ξ worker_id
    ,   visitorSupervisorIncomingWorkerRemovedEvent :: Event ξ worker_id
    ,   visitorSupervisorIncomingWorkerStatusUpdateEvent :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStatusUpdate α)))
    ,   visitorSupervisorIncomingWorkerWorkloadStolenEvent :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStolenWorkload α)))
    ,   visitorSupervisorIncomingWorkerFinishedEvent :: Event ξ (WorkerIdTagged worker_id (VisitorStatusUpdate α))
    ,   visitorSupervisorIncomingRequestFullCheckpointEvent :: Event ξ ()
    }
-- @+node:gcross.20111219132352.1423: *3* VisitorSupervisorOutgoingEvents
data VisitorSupervisorOutgoingEvents ξ worker_id α = VisitorSupervisorOutgoingEvents
    {   visitorSupervisorOutgoingWorkloadEvent :: Event ξ (WorkerIdTagged worker_id VisitorWorkload)
    ,   visitorSupervisorOutgoingFinishedEvent :: Event ξ (Seq (VisitorSolution α))
    ,   visitorSupervisorOutgoingBroadcastWorkerRequestEvent :: Event ξ ([worker_id],VisitorWorkerReactiveRequest)
    ,   visitorSupervisorOutgoingCheckpointCompleteEvent :: Event ξ (VisitorStatusUpdate α)
    ,   visitorSupervisorOutgoingNewSolutionsFoundEvent :: Event ξ (Seq (VisitorSolution α))
    ,   visitorSupervisorCurrentStatus :: Discrete ξ (VisitorStatusUpdate α)
    }
-- @+node:gcross.20111219132352.1424: ** Functions
-- @+node:gcross.20111219132352.1425: *3* createVisitorSupervisorReactiveNetwork
createVisitorSupervisorReactiveNetwork ::
    ∀ α ξ worker_id. (FRP ξ, Ord worker_id, Show worker_id) ⇒
    VisitorSupervisorIncomingEvents ξ worker_id α →
    VisitorSupervisorOutgoingEvents ξ worker_id α
createVisitorSupervisorReactiveNetwork VisitorSupervisorIncomingEvents{..} = VisitorSupervisorOutgoingEvents{..}
  where
    -- @+others
    -- @+node:gcross.20111227142510.1840: *4* Network termination
    network_has_finished :: Event ξ (Seq (VisitorSolution α))
    network_has_finished =
        filterJust
        $
        (\current_status active_workers waiting_workers_or_available_workloads (WorkerIdTagged worker_id update) →
            if (not . Map.null . Map.delete worker_id) active_workers
            || either (const False) (not . Set.null) waiting_workers_or_available_workloads
            then Nothing
            else Just (
                case current_status ⊕ update of
                    VisitorStatusUpdate Explored all_solutions → all_solutions
                    VisitorStatusUpdate remaining_checkpoint _ → throw (IncompleteVisitError remaining_checkpoint)
            )
        ) <$> current_status
          <*> active_workers
          <*> waiting_workers_or_available_workloads
          <@> visitorSupervisorIncomingWorkerFinishedEvent

    visitorSupervisorOutgoingFinishedEvent = network_has_finished
    -- @+node:gcross.20111227142510.1841: *4* Request broadcasting
    visitorSupervisorOutgoingBroadcastWorkerRequestEvent =
        (\active_worker_ids request → (Set.toList active_worker_ids,request))
            <$> active_worker_ids
            <@> (
                    (StatusUpdateReactiveRequest <$ visitorSupervisorIncomingRequestFullCheckpointEvent)
                  ⊕ (WorkloadStealReactiveRequest <$ steal_workloads_event)
                )
    -- @+node:gcross.20111227142510.1838: *4* Status updates and checkpointing
    worker_progress_status_updated_event :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStatusUpdate α)))
    worker_progress_status_updated_event =
         visitorSupervisorIncomingWorkerStatusUpdateEvent
      ⊕ (modifyTaggedDataWith (visitorWorkerStolenWorkerStatusUpdate <$>) <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent)

    worker_status_updated_event :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorStatusUpdate α)))
    worker_status_updated_event =
        (modifyTaggedDataWith (visitorWorkerStatusUpdate <$>) <$> worker_progress_status_updated_event)
      ⊕ (modifyTaggedDataWith Just <$> visitorSupervisorIncomingWorkerFinishedEvent)

    visitorSupervisorOutgoingNewSolutionsFoundEvent =
        (\WorkerIdTagged{..} →
            case workerIdTaggedData of
                Just VisitorStatusUpdate{..}
                  | (not . Seq.null) visitorStatusNewSolutions
                  → Just visitorStatusNewSolutions
                _ → Nothing
        ) <$?> worker_status_updated_event

    current_status :: Discrete ξ (VisitorStatusUpdate α)
    current_status =
        accumD mempty
        .
        (maybe id (⊕) . workerIdTaggedData <$>)
        $
        worker_status_updated_event

    visitorSupervisorCurrentStatus = current_status

    workers_with_pending_status_updates :: Discrete ξ (Set worker_id)
    workers_with_pending_status_updates = accumD Set.empty $
        (Set.union <$> active_worker_ids <@ visitorSupervisorIncomingRequestFullCheckpointEvent)
      ⊕ (Set.delete . workerId <$> worker_status_updated_event)

    full_status_update_in_progress :: Behavior ξ Bool
    full_status_update_in_progress = stepper False $
        (False <$ full_status_update_has_finished)
      ⊕ (True <$  visitorSupervisorIncomingRequestFullCheckpointEvent)

    full_status_update_has_finished :: Event ξ (VisitorStatusUpdate α)
    full_status_update_has_finished =
        filterJust . whenE full_status_update_in_progress . changes
        $
        (\current_status remaining_workers →
            if Set.null remaining_workers
                then Just current_status
                else Nothing
        )
        <$> current_status
        <*> workers_with_pending_status_updates

    visitorSupervisorOutgoingCheckpointCompleteEvent = full_status_update_has_finished
    -- @+node:gcross.20120101200431.1879: *4* Worker tracking
    active_workers :: Discrete ξ (Map worker_id VisitorWorkload)
    active_workers = accumD Map.empty . mconcat $
        [Map.delete <$> (visitorSupervisorIncomingWorkerRemovedEvent ⊕ (workerId <$> visitorSupervisorIncomingWorkerFinishedEvent))
        ,(\(WorkerIdTagged worker_id workload) →
            Map.insert worker_id workload
         ) <$> worker_deployed
        ,(\(WorkerIdTagged worker_id maybe_update) →
            maybe (Map.delete worker_id) (Map.insert worker_id . visitorWorkerRemainingWorkload) maybe_update
         ) <$> worker_progress_status_updated_event
        ]

    active_worker_ids :: Discrete ξ (Set worker_id)
    active_worker_ids = Map.keysSet <$> active_workers
    -- @+node:gcross.20120101200431.1878: *4* Workload stealing
    workers_with_pending_workload_steals :: Discrete ξ (Set worker_id)
    workers_with_pending_workload_steals = accumD (Set.empty) . mconcat $
        [Set.union <$> active_worker_ids <@ steal_workloads_event
        ,Set.delete . workerId <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent
        ,Set.delete <$> visitorSupervisorIncomingWorkerRemovedEvent
        ]

    (worker_id_without_stolen_workload,worker_id_with_stolen_workload) =
        (\(WorkerIdTagged worker_id maybe_stolen_workload) →
            case maybe_stolen_workload of
                Nothing → Left worker_id
                Just _ → Right worker_id
        ) <$↔> visitorSupervisorIncomingWorkerWorkloadStolenEvent

    steal_workloads_event :: Event ξ ()
    steal_workloads_event =
        void . filterE id . mconcat $
        [applyOutOfPendingWorkersStealCondition 1
            <$> workers_with_pending_workload_steals
            <*> waiting_workers_or_available_workloads
            <@> worker_id_with_stolen_workload
        ,applyOutOfPendingWorkersStealCondition 0
            <$> workers_with_pending_workload_steals
            <*> waiting_workers_or_available_workloads
            <@> (visitorSupervisorIncomingWorkerRemovedEvent ⊕ worker_id_without_stolen_workload)
        ,applyWorkloadRequestedStealCondition
            <$> workers_with_pending_workload_steals
            <*> waiting_workers_or_available_workloads
            <@> void workload_requested
        ]
      where
        applyOutOfPendingWorkersStealCondition
            minimum_waiting_workers
            workers_with_pending_workload_steals
            waiting_workers_or_available_workloads
            worker_id
          = (Set.null . Set.delete worker_id) workers_with_pending_workload_steals
         && either ((> minimum_waiting_workers) . Seq.length) (const False) waiting_workers_or_available_workloads

        applyWorkloadRequestedStealCondition
            workers_with_pending_workload_steals
            waiting_workers_or_available_workloads
            _
          = Set.null workers_with_pending_workload_steals
         && either (const True) Set.null waiting_workers_or_available_workloads
    -- @+node:gcross.20111227142510.1839: *4* Workload tracking and queueing
    waiting_workers_or_available_workloads :: Discrete ξ (Either (Seq worker_id) (Set VisitorWorkload))
    waiting_workers_or_available_workloads = stepperD (Right (Set.singleton entire_workload)) $
        (Left <$> (new_waiting_workers_event_1 ⊕ new_waiting_workers_event_2))
      ⊕ (Right <$> (new_available_workloads_event_1 ⊕ new_available_workloads_event_2))

    worker_deployed_1 = fst <$> worker_deployed_and_queue_updated_1
    new_available_workloads_event_1 = snd <$> worker_deployed_and_queue_updated_1
    (new_waiting_workers_event_1,worker_deployed_and_queue_updated_1) =
        (\waiting_workers_or_available_workloads worker_id →
            case waiting_workers_or_available_workloads of
                Left waiting_workers → Left (waiting_workers |> worker_id)
                Right (Set.minView → Nothing) → Left (Seq.singleton worker_id)
                Right (Set.minView → Just (workload,remaining_workloads)) → Right (WorkerIdTagged worker_id workload,remaining_workloads)
        ) <$>  waiting_workers_or_available_workloads
          <@↔> workload_requested

    worker_deployed_2 = fst <$> worker_deployed_and_queue_updated_2
    new_waiting_workers_event_2 = snd <$> worker_deployed_and_queue_updated_2
    (worker_deployed_and_queue_updated_2,new_available_workloads_event_2) =
        (\waiting_workers_or_available_workloads workload →
            case waiting_workers_or_available_workloads of
                Left (Seq.viewl → EmptyL) → Right (Set.singleton workload)
                Left (Seq.viewl → worker_id :< remaining_workers) → Left (WorkerIdTagged worker_id workload,remaining_workers)
                Right available_workloads → Right (Set.insert workload available_workloads)
        ) <$>  waiting_workers_or_available_workloads
          <@↔> workload_arrived

    workload_requested :: Event ξ worker_id
    workload_requested =
        (workerId <$> visitorSupervisorIncomingWorkerFinishedEvent)
      ⊕  visitorSupervisorIncomingWorkerAddedEvent

    workload_arrived :: Event ξ VisitorWorkload
    workload_arrived =
        filterJust (
            ((visitorWorkerStolenWorkload <$>) . workerIdTaggedData <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent)
          ⊕ (flip Map.lookup <$> active_workers <@> visitorSupervisorIncomingWorkerRemovedEvent)
        )

    worker_deployed = worker_deployed_1 ⊕ worker_deployed_2

    visitorSupervisorOutgoingWorkloadEvent = worker_deployed
    -- @-others
-- @+node:gcross.20111226153030.1437: *3* modifyTaggedDataWith
modifyTaggedDataWith :: (α → β) → WorkerIdTagged worker_id α → WorkerIdTagged worker_id β
modifyTaggedDataWith f tx = tx {workerIdTaggedData = f (workerIdTaggedData tx)}
-- @-others
-- @-leo
