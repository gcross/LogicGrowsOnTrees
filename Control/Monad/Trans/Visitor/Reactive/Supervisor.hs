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
import Control.Exception (Exception(toException),SomeException)
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
-- @+node:gcross.20111223212822.1433: ** Exceptions
-- @+node:gcross.20111223212822.1434: *3* VisitorException
data VisitorException =
    UnexploredSpaceRemainsAfterSearchException VisitorCheckpoint
  | VisitorFailed String
  deriving (Eq,Show,Typeable)

instance Exception VisitorException
-- @+node:gcross.20111219132352.1420: ** Types
-- @+node:gcross.20111226153030.1436: *3* WorkerIdTagged
data WorkerIdTagged worker_id α = WorkerIdTagged
    {   workerId :: worker_id
    ,   workerIdTaggedData :: α
    } deriving (Eq,Show)
-- @+node:gcross.20111219132352.1421: *3* VisitorSupervisorIncomingEvents
data VisitorSupervisorIncomingEvents ξ worker_id α = VisitorSupervisorIncomingEvents
    {   visitorSupervisorIncomingWorkerRecruitedEvent :: Event ξ worker_id
    ,   visitorSupervisorIncomingWorkerShutdownEvent :: Event ξ worker_id
    ,   visitorSupervisorIncomingWorkerStatusUpdateEvent :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStatusUpdate α)))
    ,   visitorSupervisorIncomingWorkerWorkloadStolenEvent :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStolenWorkload α)))
    ,   visitorSupervisorIncomingWorkerFinishedEvent :: Event ξ (WorkerIdTagged worker_id (VisitorStatusUpdate α))
    ,   visitorSupervisorIncomingWorkerFailedEvent :: Event ξ (WorkerIdTagged worker_id String)
    ,   visitorSupervisorIncomingRequestCurrentCheckpointEvent :: Event ξ ()
    ,   visitorSupervisorIncomingRequestFullCheckpointEvent :: Event ξ ()
    ,   visitorSupervisorIncomingRequestShutdownEvent :: Event ξ ()
    ,   visitorSupervisorIncomingAllRecruitmentEndedEvent :: Event ξ ()
    }
-- @+node:gcross.20111219132352.1423: *3* VisitorSupervisorOutgoingEvents
data VisitorSupervisorOutgoingEvents ξ worker_id α = VisitorSupervisorOutgoingEvents
    {   visitorSupervisorOutgoingWorkloadEvent :: Event ξ (WorkerIdTagged worker_id VisitorWorkload)
    ,   visitorSupervisorOutgoingTerminatedEvent :: Event ξ (Either VisitorException (Either (VisitorStatusUpdate α) (Seq (VisitorSolution α))))
    ,   visitorSupervisorOutgoingShutdownWorkersEvent :: Event ξ (Set worker_id)
    ,   visitorSupervisorOutgoingShutdownCompleteEvent :: Event ξ ()
    ,   visitorSupervisorOutgoingBroadcastWorkerRequestEvent :: Event ξ ([worker_id],VisitorWorkerReactiveRequest)
    ,   visitorSupervisorOutgoingStatusUpdateEvent :: Event ξ (VisitorStatusUpdate α)
    ,   visitorSupervisorOutgoingNewSolutionsEvent :: Event ξ (Seq (VisitorSolution α))
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
    -- @+node:gcross.20111227142510.1838: *4* Current status
    worker_progress_status_updated_event :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorWorkerStatusUpdate α)))
    worker_progress_status_updated_event =
         visitorSupervisorIncomingWorkerStatusUpdateEvent
      ⊕ (modifyTaggedDataWith (visitorWorkerStolenWorkerStatusUpdate <$>) <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent)

    worker_status_updated_event :: Event ξ (WorkerIdTagged worker_id (Maybe (VisitorStatusUpdate α)))
    worker_status_updated_event =
        (modifyTaggedDataWith (visitorWorkerStatusUpdate <$>) <$> worker_progress_status_updated_event)
      ⊕ (modifyTaggedDataWith Just <$> visitorSupervisorIncomingWorkerFinishedEvent)

    visitorSupervisorOutgoingNewSolutionsEvent =
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
        .
        whenNetworkIsAlive
        $
        worker_status_updated_event

    workers_with_pending_status_updates :: Discrete ξ (Set worker_id)
    workers_with_pending_status_updates = accumD (Set.empty) . whenNetworkIsAlive . mconcat $
        [Set.union <$> active_worker_ids <@ whenNetworkIsAlive visitorSupervisorIncomingRequestFullCheckpointEvent
        ,Set.delete . workerId <$> worker_status_updated_event
        ,Set.delete <$> visitorSupervisorIncomingWorkerShutdownEvent
        ,const Set.empty <$ network_has_died
        ]

    full_status_update_in_progress :: Behavior ξ Bool
    full_status_update_in_progress = stepper False $
        mconcat
        [False <$ full_status_update_has_finished
        ,False <$ network_has_died
        ,True <$ whenNetworkIsAlive visitorSupervisorIncomingRequestFullCheckpointEvent
        ]

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

    visitorSupervisorOutgoingStatusUpdateEvent =
        full_status_update_has_finished
      ⊕ (current_status <@
            (   visitorSupervisorIncomingRequestCurrentCheckpointEvent
              ⊕ whenNetworkIsDead visitorSupervisorIncomingRequestFullCheckpointEvent
            )
        )
    -- @+node:gcross.20111227142510.1840: *4* Network termination
    network_has_finished :: Event ξ (VisitorStatusUpdate α)
    network_has_finished =
        filterJust
        $
        (\current_status active_workers waiting_workers_or_available_workloads (WorkerIdTagged worker_id update) →
            if (not . Map.null . Map.delete worker_id) active_workers
            || either (const False) (not . Seq.null) waiting_workers_or_available_workloads
            then Nothing
            else Just (current_status ⊕ update)
        ) <$> current_status
          <*> active_workers
          <*> waiting_workers_or_available_workloads
          <@> visitorSupervisorIncomingWorkerFinishedEvent

    (unexplored_space_remains_after_search_event,network_has_succeeded) =
        (\VisitorStatusUpdate{..} →
            case visitorStatusCheckpoint of
                Explored → Right visitorStatusNewSolutions
                _ → Left (UnexploredSpaceRemainsAfterSearchException visitorStatusCheckpoint)
        ) <$↔> network_has_finished

    network_has_failed :: Event ξ VisitorException
    network_has_failed = mconcat
        [VisitorFailed . workerIdTaggedData <$> visitorSupervisorIncomingWorkerFailedEvent
        ,unexplored_space_remains_after_search_event
        ]

    network_has_died :: Event ξ (Either VisitorException (Either (VisitorStatusUpdate α) (Seq (VisitorSolution α))))
    network_has_died =
        whenNetworkIsAlive
        .
        mconcat
        $
        [Left <$> network_has_failed
        ,(Right . Left) <$> current_status <@ visitorSupervisorIncomingRequestShutdownEvent
        ,(Right . Right) <$> network_has_succeeded
        ]

    visitorSupervisorOutgoingTerminatedEvent = network_has_died

    network_is_alive = accumD True (const False <$ network_has_died)
    network_is_dead = not <$> network_is_alive

    whenNetworkIsAlive = whenE (value network_is_alive)
    whenNetworkIsDead = whenE (value network_is_dead)

    visitorSupervisorOutgoingShutdownWorkersEvent =
        (all_known_worker_ids <@ network_has_died)
      ⊕ (whenNetworkIsDead $ Set.singleton <$> visitorSupervisorIncomingWorkerRecruitedEvent)

    worker_ids_pending_shutdown :: Discrete ξ (Set worker_id)
    worker_ids_pending_shutdown = accumD Set.empty $
        (Set.union <$> visitorSupervisorOutgoingShutdownWorkersEvent)
      ⊕ (Set.delete <$> visitorSupervisorIncomingWorkerShutdownEvent)

    all_recruitment_has_ended :: Discrete ξ Bool
    all_recruitment_has_ended = stepperD False (True <$ visitorSupervisorIncomingAllRecruitmentEndedEvent)

    visitorSupervisorOutgoingShutdownCompleteEvent =
      (
        (\worker_ids_pending_shutdown all_recruitment_has_ended worker_id →
            if all_recruitment_has_ended && (Set.null . Set.delete worker_id $ worker_ids_pending_shutdown)
                then Just ()
                else Nothing
        ) <$>  worker_ids_pending_shutdown
          <*>  all_recruitment_has_ended
          <@?> visitorSupervisorIncomingWorkerShutdownEvent
      ) ⊕ (
        (\worker_ids_pending_shutdown all_known_worker_ids _ →
            if Set.null (worker_ids_pending_shutdown ⊕ all_known_worker_ids)
                then Just ()
                else Nothing
        ) <$>  worker_ids_pending_shutdown
          <*>  all_known_worker_ids
          <@?> visitorSupervisorIncomingAllRecruitmentEndedEvent
      )
    -- @+node:gcross.20111227142510.1841: *4* Request broadcasts
    visitorSupervisorOutgoingBroadcastWorkerRequestEvent =
        (\active_worker_ids request → (Set.toList active_worker_ids,request))
            <$> active_worker_ids
            <@> (
                    (StatusUpdateReactiveRequest <$ visitorSupervisorIncomingRequestFullCheckpointEvent)
                  ⊕ (WorkloadStealReactiveRequest <$ steal_workloads_event)
                )
    -- @+node:gcross.20111227142510.1839: *4* Worker activity and workload availability
    active_workers :: Discrete ξ (Map worker_id VisitorWorkload)
    active_workers = accumD Map.empty . mconcat $
        [Map.delete <$> (visitorSupervisorIncomingWorkerShutdownEvent ⊕ (workerId <$> visitorSupervisorIncomingWorkerFinishedEvent))
        ,(\(WorkerIdTagged worker_id workload) →
            Map.insert worker_id workload
         ) <$> worker_deployed
        ,(\(WorkerIdTagged worker_id maybe_update) →
            maybe (Map.delete worker_id) (Map.insert worker_id . visitorWorkerRemainingWorkload) maybe_update
         ) <$> worker_progress_status_updated_event
        ,const Map.empty <$ network_has_died
        ]

    active_worker_ids :: Discrete ξ (Set worker_id)
    active_worker_ids = Map.keysSet <$> active_workers

    waiting_workers_or_available_workloads :: Discrete ξ (Either (Seq worker_id) (Seq VisitorWorkload))
    waiting_workers_or_available_workloads = stepperD (Right (Seq.singleton entire_workload)) $
        (Left <$> (new_waiting_workers_event_1 ⊕ new_waiting_workers_event_2))
      ⊕ (Right <$> (new_available_workloads_event_1 ⊕ new_available_workloads_event_2))
      ⊕ (Right Seq.empty <$ network_has_failed)

    worker_deployed_1 = fst <$> worker_deployed_and_queue_updated_1
    new_available_workloads_event_1 = snd <$> worker_deployed_and_queue_updated_1
    (new_waiting_workers_event_1,worker_deployed_and_queue_updated_1) =
        (\waiting_workers_or_available_workloads worker_id →
            case waiting_workers_or_available_workloads of
                Left waiting_workers → Left (waiting_workers |> worker_id)
                Right (Seq.viewl → EmptyL) → Left (Seq.singleton worker_id)
                Right (Seq.viewl → workload :< remaining_workloads) → Right (WorkerIdTagged worker_id workload,remaining_workloads)
        ) <$>  waiting_workers_or_available_workloads
          <@↔> workload_requested

    worker_deployed_2 = fst <$> worker_deployed_and_queue_updated_2
    new_waiting_workers_event_2 = snd <$> worker_deployed_and_queue_updated_2
    (worker_deployed_and_queue_updated_2,new_available_workloads_event_2) =
        (\waiting_workers_or_available_workloads workload →
            case waiting_workers_or_available_workloads of
                Left (Seq.viewl → EmptyL) → Right (Seq.singleton workload)
                Left (Seq.viewl → worker_id :< remaining_workers) → Left (WorkerIdTagged worker_id workload,remaining_workers)
                Right available_workloads → Right (available_workloads |> workload)
        ) <$>  waiting_workers_or_available_workloads
          <@↔> workload_arrived

    workload_requested :: Event ξ worker_id
    workload_requested =
        whenNetworkIsAlive (
            fmap workerId visitorSupervisorIncomingWorkerFinishedEvent
          ⊕ visitorSupervisorIncomingWorkerRecruitedEvent
        )

    workload_arrived :: Event ξ VisitorWorkload
    workload_arrived =
        filterJust . whenNetworkIsAlive $ (
            ((visitorWorkerStolenWorkload <$>) . workerIdTaggedData <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent)
          ⊕ (flip Map.lookup <$> active_workers <@> visitorSupervisorIncomingWorkerShutdownEvent)
        )

    worker_deployed = worker_deployed_1 ⊕ worker_deployed_2

    visitorSupervisorOutgoingWorkloadEvent = worker_deployed

    workers_with_pending_workload_steals :: Discrete ξ (Set worker_id)
    workers_with_pending_workload_steals = accumD (Set.empty) . whenNetworkIsAlive . mconcat $
        [Set.union <$> active_worker_ids <@ whenNetworkIsAlive steal_workloads_event
        ,Set.delete . workerId <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent
        ,Set.delete <$> visitorSupervisorIncomingWorkerShutdownEvent
        ,const Set.empty <$ network_has_died
        ]

    steal_workloads_event :: Event ξ ()
    steal_workloads_event =
        void . filterE id . mconcat $
        [applyOutOfPendingWorkersStealCondition 1
            <$> workers_with_pending_workload_steals
            <*> waiting_workers_or_available_workloads
            <@> (workerId <$> visitorSupervisorIncomingWorkerWorkloadStolenEvent)
        ,applyOutOfPendingWorkersStealCondition 0
            <$> workers_with_pending_workload_steals
            <*> waiting_workers_or_available_workloads
            <@> visitorSupervisorIncomingWorkerShutdownEvent
        ,applyWorkloadRequestedStealCondition
            <$> workers_with_pending_workload_steals
            <*> waiting_workers_or_available_workloads
            <@> (void workload_requested)
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
         && either (const True) Seq.null waiting_workers_or_available_workloads

    waiting_worker_ids :: Discrete ξ (Set worker_id)
    waiting_worker_ids =
        either (Set.fromList . Fold.toList) mempty
        <$>
        waiting_workers_or_available_workloads

    all_known_worker_ids :: Discrete ξ (Set worker_id)
    all_known_worker_ids = (⊕) <$> waiting_worker_ids <*> active_worker_ids
    -- @-others
-- @+node:gcross.20111226153030.1437: *3* modifyTaggedDataWith
modifyTaggedDataWith :: (α → β) → WorkerIdTagged worker_id α → WorkerIdTagged worker_id β
modifyTaggedDataWith f tx = tx {workerIdTaggedData = f (workerIdTaggedData tx)}
-- @-others
-- @-leo
