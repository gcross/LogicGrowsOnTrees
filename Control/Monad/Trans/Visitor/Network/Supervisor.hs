-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Network.Supervisor where

-- Imports {{{
import Data.Accessor ((^.),(^=),(^:))
import Data.Accessor.Template (deriveAccessors)
import Control.Exception (Exception,assert,throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO,liftIO)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq,ViewL(..),(|>),viewl)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Exceptions {{{

data SupervisorError worker_id = -- {{{
    WorkerAlreadyKnown worker_id
  | WorkerNotKnown worker_id
  deriving (Eq,Show,Typeable)

instance (Eq worker_id, Show worker_id, Typeable worker_id) ⇒ Exception (SupervisorError worker_id)
-- }}}

-- }}}

-- Classes {{{

class (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id) ⇒ WorkerId worker_id where {}

-- }}}

-- Types {{{

-- Actions {{
type BroadcastWorkloadStealAction worker_id m = [worker_id] → m ()
type SendWorkloadAction worker_id m = VisitorWorkload → worker_id → m ()
type ShutdownAction worker_id m = worker_id → m ()
type SnapshotNetworkStatusAction α m = VisitorStatusUpdate α → m () 
-- }}}

data VisitorNetworkSupervisorState α worker_id = -- {{{
    VisitorNetworkSupervisorState
    {   waiting_workers_or_available_workloads_ :: !(Either (Seq worker_id) (Set VisitorWorkload))
    ,   known_workers_ :: !(Set worker_id)
    ,   active_workers_ :: !(Map worker_id VisitorWorkload)
    ,   workers_pending_workload_steal_ :: !(Set worker_id)
    ,   workers_pending_status_update_ :: !(Set worker_id)
    ,   current_status_ :: !(VisitorStatusUpdate α)
    }
$( deriveAccessors ''VisitorNetworkSupervisorState )
-- }}}

-- }}}

-- Values {{{

initial_supervisor_state :: (Monoid α, Ord worker_id) ⇒ VisitorNetworkSupervisorState α worker_id
initial_supervisor_state = VisitorNetworkSupervisorState -- {{{
    {   waiting_workers_or_available_workloads_ = Right (Set.singleton entire_workload)
    ,   known_workers_ = mempty
    ,   active_workers_ = mempty
    ,   workers_pending_workload_steal_ = mempty
    ,   workers_pending_status_update_ = mempty
    ,   current_status_ = mempty
    }
-- }}}

-- }}}

-- Functions {{{

enqueueWorkload :: -- {{{
    (MonadIO m, WorkerId worker_id) ⇒
    SendWorkloadAction worker_id m →
    VisitorWorkload →
    VisitorNetworkSupervisorState α worker_id →
    m (VisitorNetworkSupervisorState α worker_id)

enqueueWorkload
    sendWorkloadToWorker
    workload
    old_state
    =
    case old_state ^. waiting_workers_or_available_workloads of
        Left (viewl → (free_worker_id :< remaining_workers)) → do
            sendWorkloadToWorker workload free_worker_id
            return
                .(waiting_workers_or_available_workloads ^= Left remaining_workers)
                .(active_workers ^: Map.insert free_worker_id workload)
                $
                old_state
        Left (viewl → EmptyL) →
            return
                .(waiting_workers_or_available_workloads ^= Right (Set.singleton workload))
                $
                old_state
        Right available_workloads →
            return
                .(waiting_workers_or_available_workloads ^= Right (Set.insert workload available_workloads))
                $
                old_state
-- }}}

tryToObtainWorkloadFor :: -- {{{
    (MonadIO m, WorkerId worker_id) ⇒
    BroadcastWorkloadStealAction worker_id m →
    SendWorkloadAction worker_id m →
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m (VisitorNetworkSupervisorState α worker_id)

tryToObtainWorkloadFor
    broadcastWorkloadStealToWorkers
    sendWorkloadToWorker
    worker_id
    old_state
    =
    case (old_state ^. waiting_workers_or_available_workloads) of
        Left waiting_workers → assert (not . Seq.null $ waiting_workers) $
            return
                .(waiting_workers_or_available_workloads ^= Left (waiting_workers |> worker_id))
                $
                old_state
        Right (Set.minView → Nothing) → do
            let broadcast_worker_ids = Map.keys (old_state ^. active_workers)
            broadcastWorkloadStealToWorkers broadcast_worker_ids
            return
                .(waiting_workers_or_available_workloads ^= Left (Seq.singleton worker_id))
                .(workers_pending_workload_steal ^= Set.fromList broadcast_worker_ids)
                $
                old_state
        Right (Set.minView → Just (workload,remaining_workloads)) → do
            sendWorkloadToWorker workload worker_id
            return
                .(waiting_workers_or_available_workloads ^= Right remaining_workloads)
                .(active_workers ^: Map.insert worker_id workload)
                $
                old_state
-- }}}

updateStatusUpdated :: -- {{{
    (MonadIO m, WorkerId worker_id, Monoid α) ⇒
    SnapshotNetworkStatusAction α m →
    VisitorStatusUpdate α →
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m (VisitorNetworkSupervisorState α worker_id)
updateStatusUpdated
    snapshotNetworkStatus
    status_update
    worker_id
    old_state
    = do
    validateWorkerKnown worker_id old_state
    return
        .(workers_pending_status_update ^: Set.delete worker_id)
        .(current_status ^: mappend status_update)
        $
        old_state
    -- NOTE:  CODE IS ***NOT*** COMPLETE
-- }}}

updateWorkerAdded :: -- {{{
    (MonadIO m, WorkerId worker_id) ⇒
    BroadcastWorkloadStealAction worker_id m →
    SendWorkloadAction worker_id m →
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m (VisitorNetworkSupervisorState α worker_id)
updateWorkerAdded
    broadcastWorkloadStealToWorkers
    sendWorkloadToWorker
    worker_id
    old_state
 | Set.member worker_id (old_state ^. known_workers) =
    liftIO . throwIO $ WorkerAlreadyKnown worker_id
 | otherwise =
    tryToObtainWorkloadFor
        broadcastWorkloadStealToWorkers
        sendWorkloadToWorker
        worker_id
        ((known_workers ^: Set.insert worker_id) old_state)
-- }}}

updateWorkerFinished ::  -- {{{
    (MonadIO m, WorkerId worker_id, Monoid α) ⇒
    BroadcastWorkloadStealAction worker_id m →
    SendWorkloadAction worker_id m →
    VisitorStatusUpdate α →
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m (Either α (VisitorNetworkSupervisorState α worker_id))

updateWorkerFinished
    broadcastWorkloadStealToWorkers
    sendWorkloadToWorker
    status_update
    worker_id
    old_state
    = do
    validateWorkerKnown worker_id old_state
    let new_status@(VisitorStatusUpdate new_checkpoint new_results) = (old_state ^. current_status) `mappend` status_update
        new_state =
             (current_status ^= new_status)
            .(active_workers ^: Map.delete worker_id)
            .(workers_pending_workload_steal ^: Set.delete worker_id)
            .(workers_pending_status_update ^: Set.delete worker_id)
            $
            old_state
    case new_checkpoint of
        Explored → return $ Left new_results
        _ → tryToObtainWorkloadFor
                broadcastWorkloadStealToWorkers
                sendWorkloadToWorker
                worker_id
                new_state
            >>= return . Right
-- }}}

updateWorkerRemoved :: -- {{{
    (MonadIO m, WorkerId worker_id) ⇒
    SendWorkloadAction worker_id m →
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m (VisitorNetworkSupervisorState α worker_id)
updateWorkerRemoved sendWorkloadToWorker worker_id old_state = do
    validateWorkerKnown worker_id old_state
    let new_state =
             (known_workers ^: Set.delete worker_id)
            .(active_workers ^: Map.delete worker_id)
            .(workers_pending_workload_steal ^: Set.delete worker_id)
            .(workers_pending_status_update ^: Set.delete worker_id)
            $
            old_state
    case Map.lookup worker_id (new_state ^. active_workers) of
        Nothing → return new_state
        Just workload →
            enqueueWorkload
                sendWorkloadToWorker
                workload
                new_state
-- }}}

updateWorkloadStolen :: -- {{{
    (MonadIO m, WorkerId worker_id) ⇒
    SendWorkloadAction worker_id m →
    VisitorWorkload →
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m (VisitorNetworkSupervisorState α worker_id)
updateWorkloadStolen
    sendWorkloadToWorker
    workload
    worker_id
    old_state
    = do
    validateWorkerKnown worker_id old_state
    let new_state = (workers_pending_workload_steal ^: Set.delete worker_id) old_state
    enqueueWorkload
        sendWorkloadToWorker
        workload
        new_state
-- }}}

validateWorkerKnown :: -- {{{
    (MonadIO m, WorkerId worker_id) ⇒
    worker_id →
    VisitorNetworkSupervisorState α worker_id →
    m ()
validateWorkerKnown worker_id state =
    when (Set.notMember worker_id (state ^. known_workers))
    .
    (liftIO . throwIO)
    $
    WorkerNotKnown worker_id
-- }}}

-- }}}
