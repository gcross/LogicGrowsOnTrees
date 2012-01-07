-- Langauge extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Reactive.Supervisor where

-- Imports {{{
import Control.Exception (Exception,throw)
import Control.Monad (void)

import qualified Data.Foldable as Fold
import Data.Functor
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isJust)
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
-- }}}

-- Exception {{{

data IncompleteVisitError = IncompleteVisitError VisitorCheckpoint deriving Typeable

instance Show IncompleteVisitError where
    show (IncompleteVisitError checkpoint) = "After visiting the entire search space, the cumulative checkpoint was not Explored but " ++ show checkpoint

instance Exception IncompleteVisitError

-- }}}

-- Types {{{

data WorkerIdTagged worker_id α = -- {{{
    WorkerIdTagged
    {   workerId :: worker_id
    ,   workerIdTaggedData :: α
    } deriving (Eq,Show)
-- }}}

data VisitorSupervisorIncomingEvents ξ worker_id α = -- {{{
    VisitorSupervisorIncomingEvents
    {   visitorSupervisorIncomingWorkerAddedEvent :: Event ξ worker_id
    ,   visitorSupervisorIncomingWorkerRemovedEvent :: Event ξ worker_id
    ,   visitorSupervisorIncomingWorkerStatusUpdateEvent :: Event ξ (WorkerIdTagged worker_id (VisitorWorkerStatusUpdate α))
    ,   visitorSupervisorIncomingWorkerWorkloadStolenEvent :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStolenWorkload α)))
    ,   visitorSupervisorIncomingWorkerFinishedEvent :: Event ξ (WorkerIdTagged worker_id (VisitorStatusUpdate α))
    ,   visitorSupervisorIncomingRequestFullCheckpointEvent :: Event ξ ()
    }
-- }}}

data VisitorSupervisorOutgoingEvents ξ worker_id α = -- {{{
    VisitorSupervisorOutgoingEvents
    {   visitorSupervisorOutgoingWorkloadEvent :: Event ξ (WorkerIdTagged worker_id VisitorWorkload)
    ,   visitorSupervisorOutgoingFinishedEvent :: Event ξ VisitorCheckpoint
    ,   visitorSupervisorOutgoingBroadcastWorkerRequestEvent :: Event ξ ([worker_id],VisitorWorkerReactiveRequest)
    ,   visitorSupervisorOutgoingCheckpointCompleteEvent :: Event ξ (VisitorStatusUpdate α)
    ,   visitorSupervisorOutgoingNewResultsFoundEvent :: Event ξ α
    ,   visitorSupervisorCurrentStatus :: Discrete ξ (VisitorStatusUpdate α)
    }
-- }}}

-- }}}

-- Functions {{{

createVisitorSupervisorReactiveNetwork :: -- {{{
    ∀ α ξ worker_id. (FRP ξ, Ord worker_id, Show worker_id, Monoid α, Eq α) ⇒
    VisitorSupervisorIncomingEvents ξ worker_id α →
    VisitorSupervisorOutgoingEvents ξ worker_id α
createVisitorSupervisorReactiveNetwork VisitorSupervisorIncomingEvents{..} = VisitorSupervisorOutgoingEvents{..}
  where
  -- Network termination {{{
    network_has_finished :: Event ξ VisitorCheckpoint
    network_has_finished =
        filterJust
        $
        (\current_status active_workers waiting_workers_or_available_workloads (WorkerIdTagged worker_id update) →
            if (not . Map.null . Map.delete worker_id) active_workers
            || either (const False) (not . Set.null) waiting_workers_or_available_workloads
            then Nothing
            else Just (visitorStatusCheckpoint (current_status ⊕ update))
        ) <$> current_status
          <*> active_workers
          <*> waiting_workers_or_available_workloads
          <@> visitorSupervisorIncomingWorkerFinishedEvent

    visitorSupervisorOutgoingFinishedEvent = network_has_finished
  -- }}}
  -- Request broadcasting {{{
    visitorSupervisorOutgoingBroadcastWorkerRequestEvent =
        (\active_worker_ids request → (Set.toList active_worker_ids,request))
            <$> active_worker_ids
            <@> (
                    (StatusUpdateReactiveRequest <$ visitorSupervisorIncomingRequestFullCheckpointEvent)
                  ⊕ (WorkloadStealReactiveRequest <$ steal_workloads_event)
                )
  -- }}}
  -- Status updates and checkpointing {{{
    worker_progress_status_updated_event :: Event ξ (WorkerIdTagged worker_id (VisitorWorkerStatusUpdate α))
    worker_progress_status_updated_event =
         visitorSupervisorIncomingWorkerStatusUpdateEvent
      ⊕ ((\(WorkerIdTagged worker_id maybe_stolen_workload) →
            WorkerIdTagged worker_id <$> (visitorWorkerStolenWorkerStatusUpdate <$> maybe_stolen_workload)
        ) <$?> visitorSupervisorIncomingWorkerWorkloadStolenEvent)

    worker_status_updated_event :: Event ξ (WorkerIdTagged worker_id (VisitorStatusUpdate α))
    worker_status_updated_event =
        (modifyTaggedDataWith visitorWorkerStatusUpdate <$> worker_progress_status_updated_event)
      ⊕  visitorSupervisorIncomingWorkerFinishedEvent

    visitorSupervisorOutgoingNewResultsFoundEvent =
        (\(WorkerIdTagged _ VisitorStatusUpdate{visitorStatusNewResults}) →
            if visitorStatusNewResults == mempty
                then Nothing
                else Just visitorStatusNewResults
        ) <$?> worker_status_updated_event

    current_status :: Discrete ξ (VisitorStatusUpdate α)
    current_status = accumD mempty $ (⊕) . workerIdTaggedData <$> worker_status_updated_event

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
  -- }}}
  -- Worker tracking {{{
    active_workers :: Discrete ξ (Map worker_id VisitorWorkload)
    active_workers = accumD Map.empty . mconcat $
        [Map.delete <$> (visitorSupervisorIncomingWorkerRemovedEvent ⊕ (workerId <$> visitorSupervisorIncomingWorkerFinishedEvent))
        ,(\(WorkerIdTagged worker_id workload) →
            Map.insert worker_id workload
         ) <$> worker_deployed
        ,(\(WorkerIdTagged worker_id update) →
            (Map.insert worker_id . visitorWorkerRemainingWorkload) update
         ) <$> worker_progress_status_updated_event
        ]

    active_worker_ids :: Discrete ξ (Set worker_id)
    active_worker_ids = Map.keysSet <$> active_workers
  -- }}}
  -- Workload stealing {{{
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
  -- }}}
  -- Workload tracking and queueing {{{
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
  -- }}}
-- }}}

modifyTaggedDataWith :: (α → β) → WorkerIdTagged worker_id α → WorkerIdTagged worker_id β -- {{{
modifyTaggedDataWith f tx = tx {workerIdTaggedData = f (workerIdTaggedData tx)}
-- }}}

-- }}}
