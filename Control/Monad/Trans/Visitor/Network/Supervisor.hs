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
import Control.Monad (unless,when)
import Control.Monad.CatchIO (MonadCatchIO,throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Abort (AbortT,abort)
import Control.Monad.Trans.RWS.Strict (RWST,asks)

import Data.Either.Unwrap (whenLeft)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (isNothing)
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
  | WorkerNotActive worker_id
  | ActiveWorkersRemainedAfterSpaceFullyExplored [worker_id]
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
        (AbortT
            (VisitorNetworkResult result worker_id)
            (VisitorNetworkSupervisorContext result worker_id m)
            a
        )
    }
-- }}}

-- }}}

-- Exposed functions {{{

getCurrentStatus :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorNetworkSupervisorMonad result worker_id m (VisitorStatusUpdate result)
getCurrentStatus = VisitorNetworkSupervisorMonad . lift . get $ current_status
-- }}}

updateStolenWorkloadReceived :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    Maybe VisitorWorkload →
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
updateStolenWorkloadReceived maybe_workload worker_id = VisitorNetworkSupervisorMonad . lift $ do
    validateWorkerKnownAndActive worker_id
    maybe (return ()) enqueueWorkload maybe_workload
    clearPendingWorkloadSteal worker_id
-- }}}

updateWorkerAdded :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
updateWorkerAdded worker_id = VisitorNetworkSupervisorMonad . lift $ do
    validateWorkerNotKnown worker_id
    known_workers %: Set.insert worker_id
    tryToObtainWorkloadFor worker_id
-- }}}

updateWorkerFinished :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorStatusUpdate result →
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
updateWorkerFinished status_update worker_id = VisitorNetworkSupervisorMonad $
    (lift $ do
        validateWorkerKnownAndActive worker_id
        active_workers %: Map.delete worker_id
        current_status %: (`mappend` status_update)
        VisitorStatusUpdate checkpoint new_results ← get current_status
        case checkpoint of
            Explored → do
                active_worker_ids ← Map.keys <$> get active_workers
                unless (null active_worker_ids) $
                    throw $ ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
                known_worker_ids ← Set.toList <$> get known_workers
                return . Just $ VisitorNetworkResult new_results known_worker_ids
            _ → do
                tryToObtainWorkloadFor worker_id
                return Nothing
    ) >>= maybe (return ()) abort
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
    isNothing . Map.lookup worker_id <$> get active_workers
        >>= flip unless (error "sending a workload to a worker already active!")
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
        >>= flip when (throw $ WorkerNotKnown worker_id)
-- }}}

validateWorkerKnownAndActive :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
validateWorkerKnownAndActive worker_id = do
    validateWorkerKnown worker_id
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotActive worker_id)
-- }}}

validateWorkerNotKnown :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorContext result worker_id m ()
validateWorkerNotKnown worker_id = do
    Set.member worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown worker_id)
-- }}}

-- }}}
