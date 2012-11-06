-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Supervisor -- {{{
    ( VisitorSupervisorActions(..)
    , VisitorSupervisorMonad
    , VisitorSupervisorResult(..)
    , abortSupervisor
    , addWorker
    , getCurrentProgress
    , getNumberOfWorkers
    , getWaitingWorkers
    , performGlobalProgressUpdate
    , receiveProgressUpdate
    , receiveStolenWorkload
    , receiveWorkerFinished
    , removeWorker
    , runVisitorSupervisor
    , runVisitorSupervisorStartingFrom
    ) where -- }}}

-- Imports {{{
import Control.Applicative ((<$>),(<*>),Applicative)
import Control.Exception (Exception,assert)
import Control.Monad (unless,when)
import Control.Monad.CatchIO (MonadCatchIO,throw)
import Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Control.Monad.State.Class as MonadsTF
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Abort (AbortT,abort,runAbortT)
import Control.Monad.Trans.RWS.Strict (RWST,asks,evalRWST)

import Data.Accessor.Monad.TF.State ((%=),(%:),get)
import Data.Accessor.Template (deriveAccessors)
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
import Control.Monad.Trans.Visitor.Worker
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

-- Types {{{

data VisitorSupervisorActions result worker_id m = -- {{{
    VisitorSupervisorActions
    {   broadcast_progress_update_to_workers_action :: [worker_id] → m ()
    ,   broadcast_workload_steal_to_workers_action :: [worker_id] → m ()
    ,   receive_current_progress_action :: VisitorProgress result → m ()
    ,   send_workload_to_worker_action :: VisitorWorkload → worker_id → m ()
    }
-- }}}

data VisitorSupervisorState result worker_id = -- {{{
    VisitorSupervisorState
    {   waiting_workers_or_available_workloads_ :: !(Either (Seq worker_id) (Set VisitorWorkload))
    ,   known_workers_ :: !(Set worker_id)
    ,   active_workers_ :: !(Map worker_id VisitorWorkload)
    ,   workers_pending_workload_steal_ :: !(Set worker_id)
    ,   workers_pending_progress_update_ :: !(Set worker_id)
    ,   current_progress_ :: !(VisitorProgress result)
    }
$( deriveAccessors ''VisitorSupervisorState )
-- }}}

data VisitorSupervisorResult result worker_id = VisitorSupervisorResult (Either (VisitorProgress result) result) [worker_id] deriving (Eq,Show)

type VisitorSupervisorContext result worker_id m = -- {{{
    RWST
        (VisitorSupervisorActions result worker_id m)
        ()
        (VisitorSupervisorState result worker_id)
        m
-- }}}

newtype VisitorSupervisorMonad result worker_id m a = -- {{{
    VisitorSupervisorMonad {
      unwrapVisitorSupervisorMonad ::
        (AbortT
            (VisitorSupervisorResult result worker_id)
            (VisitorSupervisorContext result worker_id m)
            a
        )
    } deriving (Applicative,Functor,Monad,MonadIO)
-- }}}

-- }}}

-- Instances {{{

instance MonadTrans (VisitorSupervisorMonad result worker_id) where -- {{{
    lift = VisitorSupervisorMonad . lift . lift
-- }}}

instance MonadsTF.MonadState m ⇒ MonadsTF.MonadState (VisitorSupervisorMonad result worker_id m) where -- {{{
    type StateType (VisitorSupervisorMonad result worker_id m) = MonadsTF.StateType m
    get = lift MonadsTF.get
    put = lift . MonadsTF.put
-- }}}

-- }}}

-- Exposed functions {{{

abortSupervisor :: (Functor m, Monad m) ⇒ VisitorSupervisorMonad result worker_id m α -- {{{
abortSupervisor = VisitorSupervisorMonad $
    (lift
     $
     VisitorSupervisorResult
        <$> (Left <$> get current_progress)
        <*> (Set.toList <$> get known_workers)
    )
    >>= abort
-- }}}

addWorker :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
addWorker worker_id = VisitorSupervisorMonad . lift $ do
    validateWorkerNotKnown worker_id
    known_workers %: Set.insert worker_id
    tryToObtainWorkloadFor worker_id
-- }}}

getCurrentProgress :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m (VisitorProgress result)
getCurrentProgress = VisitorSupervisorMonad . lift . get $ current_progress
-- }}}

getNumberOfWorkers :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m Int
getNumberOfWorkers = VisitorSupervisorMonad . lift . (Set.size <$>) . get $ known_workers
-- }}}

getWaitingWorkers :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m (Seq worker_id)
getWaitingWorkers = VisitorSupervisorMonad . lift $
    get waiting_workers_or_available_workloads
    >>=
    \x → case x of
        Left waiting_workers → return waiting_workers
        Right _ → return Seq.empty
-- }}}

performGlobalProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m ()
performGlobalProgressUpdate = VisitorSupervisorMonad . lift $ do
    active_worker_ids ← Map.keysSet <$> get active_workers
    if (Set.null active_worker_ids)
        then receiveCurrentProgress
        else do
            workers_pending_progress_update %= active_worker_ids
            asks broadcast_progress_update_to_workers_action >>= lift . ($ Set.toList active_worker_ids)
-- }}}

receiveProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    Maybe (VisitorWorkerProgressUpdate result) →
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
receiveProgressUpdate maybe_update worker_id = VisitorSupervisorMonad $ do
    (VisitorProgress checkpoint result) ← lift $ do
        validateWorkerKnownAndActive worker_id
        case maybe_update of
            Nothing → return ()
            Just (VisitorWorkerProgressUpdate progress_update remaining_workload) → do
                current_progress %: (`mappend` progress_update)
                active_workers %: (Map.insert worker_id remaining_workload)
        clearPendingProgressUpdate worker_id
        get current_progress
    when (checkpoint == Explored) $
        lift (Set.toList <$> get known_workers)
            >>= abort . VisitorSupervisorResult (Right result) 
-- }}}

receiveStolenWorkload :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    Maybe (VisitorWorkerStolenWorkload result) →
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
receiveStolenWorkload maybe_stolen_workload worker_id = VisitorSupervisorMonad . lift $ do
    validateWorkerKnownAndActive worker_id
    case maybe_stolen_workload of
        Nothing → return ()
        Just (VisitorWorkerStolenWorkload (VisitorWorkerProgressUpdate progress_update remaining_workload) workload) → do
            current_progress %: (`mappend` progress_update)
            active_workers %: (Map.insert worker_id remaining_workload)
            enqueueWorkload workload
    clearPendingWorkloadSteal worker_id
-- }}}

receiveWorkerFinished :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorProgress result →
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
receiveWorkerFinished final_progress worker_id = VisitorSupervisorMonad $
    (lift $ do
        validateWorkerKnownAndActive worker_id
        active_workers %: Map.delete worker_id
        current_progress %: (`mappend` final_progress)
        VisitorProgress checkpoint new_results ← get current_progress
        case checkpoint of
            Explored → do
                active_worker_ids ← Map.keys <$> get active_workers
                unless (null active_worker_ids) $
                    throw $ ActiveWorkersRemainedAfterSpaceFullyExplored active_worker_ids
                known_worker_ids ← Set.toList <$> get known_workers
                return . Just $ VisitorSupervisorResult (Right new_results) known_worker_ids
            _ → do
                tryToObtainWorkloadFor worker_id
                return Nothing
    ) >>= maybe (return ()) abort
-- }}}

removeWorker :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorMonad result worker_id m ()
removeWorker worker_id = VisitorSupervisorMonad . lift $ do
    validateWorkerKnown worker_id 
    Map.lookup worker_id <$> get active_workers >>= maybe (return ()) enqueueWorkload
    known_workers %: Set.delete worker_id
    active_workers %: Map.delete worker_id
    clearPendingWorkloadSteal worker_id
    clearPendingProgressUpdate worker_id
-- }}}

runVisitorSupervisor :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorActions result worker_id m →
    (∀ a. VisitorSupervisorMonad result worker_id m a) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisor = runVisitorSupervisorStartingFrom mempty
-- }}}

runVisitorSupervisorStartingFrom :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorProgress result →
    VisitorSupervisorActions result worker_id m →
    (∀ a. VisitorSupervisorMonad result worker_id m a) →
    m (VisitorSupervisorResult result worker_id)
runVisitorSupervisorStartingFrom starting_progress actions loop =
    (fst <$>)
    .
    (\x ->
     evalRWST
        x
        actions
        (VisitorSupervisorState
            {   waiting_workers_or_available_workloads_ =
                    Right (Set.singleton (VisitorWorkload Seq.empty (visitorCheckpoint starting_progress)))
            ,   known_workers_ = mempty
            ,   active_workers_ = mempty
            ,   workers_pending_workload_steal_ = mempty
            ,   workers_pending_progress_update_ = mempty
            ,   current_progress_ = starting_progress
            }
        )
    )
    .
    runAbortT
    .
    unwrapVisitorSupervisorMonad
    $
    loop'
  where
    loop' = loop
-- }}}

-- }}}

-- Internal Functions {{{

broadcastWorkloadStealToActiveWorkers :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m ()
broadcastWorkloadStealToActiveWorkers = do
    active_worker_ids ← Map.keysSet <$> get active_workers
    when (Set.null active_worker_ids) $ error "no workers to broadcast!"
    asks broadcast_workload_steal_to_workers_action >>= lift . ($ Set.toList active_worker_ids)
    workers_pending_workload_steal %= active_worker_ids
-- }}}

clearPendingProgressUpdate :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
clearPendingProgressUpdate worker_id =
    Set.member worker_id <$> get workers_pending_progress_update >>= flip when
    -- Note, the conditional above is needed to prevent a "misfire" where
    -- we think that we have just completed a progress update even though
    -- none was started.
    (do workers_pending_progress_update %: Set.delete worker_id
        no_progress_updates_are_pending ← Set.null <$> get workers_pending_progress_update
        when no_progress_updates_are_pending receiveCurrentProgress
    )
-- }}}

clearPendingWorkloadSteal :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
clearPendingWorkloadSteal worker_id =
    Set.member worker_id <$> get workers_pending_workload_steal >>= flip when
    -- Note, the conditional above is needed to prevent a "misfire" where
    -- we think that we have just completed a workload steal even though
    -- none was started.
    (do workers_pending_workload_steal %: Set.delete worker_id
        no_workload_steals_remain ← Set.null <$> get workers_pending_workload_steal
        workers_are_waiting_for_workloads ← either (not . Seq.null) (const False) <$> get waiting_workers_or_available_workloads
        when (no_workload_steals_remain && workers_are_waiting_for_workloads) broadcastWorkloadStealToActiveWorkers
    )
-- }}}

enqueueWorkload :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    VisitorSupervisorContext result worker_id m ()
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

receiveCurrentProgress :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorContext result worker_id m ()
receiveCurrentProgress = do
    callback ← asks receive_current_progress_action
    current_progress ← get current_progress
    lift (callback current_progress)
-- }}}

sendWorkloadToWorker :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorWorkload →
    worker_id →
    VisitorSupervisorContext result worker_id m ()
sendWorkloadToWorker workload worker_id = do
    asks send_workload_to_worker_action >>= lift . (\f → f workload worker_id)
    isNothing . Map.lookup worker_id <$> get active_workers
        >>= flip unless (error "sending a workload to a worker already active!")
    active_workers %: Map.insert worker_id workload
-- }}}

tryToObtainWorkloadFor :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
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
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerKnown worker_id =
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotKnown worker_id)
-- }}}

validateWorkerKnownAndActive :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerKnownAndActive worker_id = do
    validateWorkerKnown worker_id
    Set.notMember worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerNotActive worker_id)
-- }}}

validateWorkerNotKnown :: -- {{{
    (Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorSupervisorContext result worker_id m ()
validateWorkerNotKnown worker_id = do
    Set.member worker_id <$> (get known_workers)
        >>= flip when (throw $ WorkerAlreadyKnown worker_id)
-- }}}

-- }}}
