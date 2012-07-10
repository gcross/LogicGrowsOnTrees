-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Network.Supervisor where

-- Imports {{{
import Data.Accessor ((^.),(^=),(^:))
import Data.Accessor.Monad.MTL.State ((%=),(%:),get)
import Data.Accessor.Template (deriveAccessors)
import Control.Applicative ((<$>))
import Control.Exception (Exception,assert)
import Control.Monad (when)
import Control.Monad.CatchIO (MonadCatchIO,throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.RWS.Strict (RWST,asks)

import Data.Either.Unwrap (whenLeft)
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

data VisitorNetworkSupervisorActions result worker_id m = -- {{{
    VisitorNetworkSupervisorActions
    {   broadcast_workload_steal_to_workers_action :: [worker_id] → m ()
    ,   receive_current_status_action :: VisitorStatusUpdate result → m ()
    ,   send_workload_to_worker_action :: VisitorWorkload → worker_id → m ()
    }
-- }}}

data VisitorNetworkSupervisorState result worker_id = -- {{{
    VisitorNetworkSupervisorState
    {   waiting_workers_or_available_workloads_ :: !(Either (Seq worker_id) (Set VisitorWorkload))
    ,   known_workers_ :: !(Set worker_id)
    ,   active_workers_ :: !(Map worker_id VisitorWorkload)
    ,   workers_pending_workload_steal_ :: !(Set worker_id)
    ,   workers_pending_status_update_ :: !(Set worker_id)
    ,   current_status_ :: !(VisitorStatusUpdate result)
    }
$( deriveAccessors ''VisitorNetworkSupervisorState )
-- }}}

data VisitorNetworkResult result worker_id = VisitorNetworkResult result [worker_id]


type VisitorNetworkSupervisorContext result worker_id m = -- {{{
    RWST
        (VisitorNetworkSupervisorActions result worker_id m)
        ()
        (VisitorNetworkSupervisorState result worker_id)
        m
-- }}}

newtype VisitorNetworkSupervisorMonad result worker_id m a = -- {{{
    VisitorNetworkSupervisorMonad {
      unwrapVisitorNetworkSupervisorMonad ::
        (ContT
            (VisitorNetworkResult result worker_id)
            (VisitorNetworkSupervisorContext result worker_id m)
            a
        )
    }
-- }}}

-- }}}

-- Exposed functions {{{

updateWorkerAdded :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
updateWorkerAdded worker_id = VisitorNetworkSupervisorMonad . lift $ do
    validateWorkerNotKnown worker_id
    known_workers %: Set.insert worker_id
    tryToObtainWorkloadFor worker_id
-- }}}

updateWorkerRemoved :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
updateWorkerRemoved worker_id = VisitorNetworkSupervisorMonad . lift $ do
    validateWorkerKnown worker_id
    maybe_workload ← Map.lookup worker_id <$> get active_workers
    known_workers %: Set.delete worker_id
    active_workers %: Map.delete worker_id
    clearPendingWorkloadSteal worker_id
    clearPendingStatusUpdate worker_id
    maybe (return ()) enqueueWorkload maybe_workload
-- }}}

updateWorkloadStolen :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
updateWorkloadStolen workload worker_id = VisitorNetworkSupervisorMonad . lift $ do
    validateWorkerKnown worker_id
    enqueueWorkload workload
    clearPendingWorkloadSteal worker_id
-- }}}

-- }}}

-- Internal Functions {{{

broadcastWorkloadStealToActiveWorkers :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorNetworkSupervisorContext result worker_id m ()
broadcastWorkloadStealToActiveWorkers = do
    active_worker_ids ← Map.keysSet <$> get active_workers
    when (Set.null active_worker_ids) $ error "no workers to broadcast!"
    asks broadcast_workload_steal_to_workers_action >>= lift . ($ Set.toList active_worker_ids)
    workers_pending_workload_steal %= active_worker_ids
-- }}}

clearPendingStatusUpdate :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
clearPendingStatusUpdate worker_id = do
    workers_pending_status_update %: Set.delete worker_id
    no_status_updates_are_pending ← Set.null <$> get workers_pending_status_update
    when no_status_updates_are_pending receiveCurrentStatus 
-- }}}

clearPendingWorkloadSteal :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
clearPendingWorkloadSteal worker_id = do
    workers_pending_workload_steal %: Set.delete worker_id
    no_workload_steals_remain ← Set.null <$> get workers_pending_workload_steal
    workers_are_waiting_for_workloads ← either (not . Seq.null) (const False) <$> get waiting_workers_or_available_workloads
    when (no_workload_steals_remain && workers_are_waiting_for_workloads) broadcastWorkloadStealToActiveWorkers
-- }}}

enqueueWorkload :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    VisitorNetworkSupervisorContext result worker_id m ()
enqueueWorkload workload =
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left (viewl → (free_worker_id :< remaining_workers)) → do
            sendWorkloadToWorker workload free_worker_id
            waiting_workers_or_available_workloads %= Left remaining_workers
        Left (viewl → EmptyL) →
            waiting_workers_or_available_workloads %= Right (Set.singleton workload)
        Right available_workloads →
            waiting_workers_or_available_workloads %= Right (Set.insert workload available_workloads)
-- }}}

receiveCurrentStatus :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorNetworkSupervisorContext result worker_id m ()
receiveCurrentStatus = do
    callback ← asks receive_current_status_action
    current_status ← get current_status
    lift (callback current_status)
-- }}}

sendWorkloadToWorker :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
sendWorkloadToWorker workload worker_id = do
    asks send_workload_to_worker_action >>= lift . (\f → f workload worker_id)
    active_workers %: Map.insert worker_id workload
-- }}}

tryToObtainWorkloadFor :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
tryToObtainWorkloadFor worker_id =
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers → assert (not . Seq.null $ waiting_workers) $
            waiting_workers_or_available_workloads %= Left (waiting_workers |> worker_id)
        Right (Set.minView → Nothing) → do
            broadcastWorkloadStealToActiveWorkers
            waiting_workers_or_available_workloads %= Left (Seq.singleton worker_id)
        Right (Set.minView → Just (workload,remaining_workloads)) → do
            sendWorkloadToWorker workload worker_id
            waiting_workers_or_available_workloads %= Right remaining_workloads
-- }}}

validateWorkerKnown :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
validateWorkerKnown worker_id =
    Set.notMember worker_id <$> (get known_workers)
    >>=
    flip when (throw $ WorkerNotKnown worker_id)
-- }}}

validateWorkerNotKnown :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
validateWorkerNotKnown worker_id =
    Set.member worker_id <$> (get known_workers)
    >>=
    flip when (throw $ WorkerAlreadyKnown worker_id)
-- }}}

-- }}}
