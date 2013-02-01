-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Parallel.Workgroup
    ( WorkerId
    , WorkgroupCallbacks(..)
    , WorkgroupControllerMonad
    , WorkgroupReceivers(..)
    , WorkgroupRequestQueueMonad(..)
    , changeNumberOfWorkers
    , runWorkgroup
    ) where

-- Imports {{{
import Control.Applicative (Applicative,(<$>))
import Control.Concurrent (forkIO,killThread)
import Control.Monad (forever,forM_,mapM_,replicateM_)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (MonadState,StateType)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT,ask,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalStateT)

import Data.Accessor.Monad.TF.State ((%=),(%:),get,getAndModify)
import Data.Accessor.Template (deriveAccessors)
import Data.Composition ((.*))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.PSQueue (Binding((:->)),PSQ)
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set
import Data.Word (Word64)

import qualified System.Log.Logger as Logger

import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Supervisor
import Control.Monad.Trans.Visitor.Supervisor.Driver (TerminationReason(..))
import Control.Monad.Trans.Visitor.Supervisor.RequestQueue
import Control.Monad.Trans.Visitor.Worker (VisitorWorkerProgressUpdate(..),VisitorWorkerStolenWorkload(..),VisitorWorkerTerminationReason(..))
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Types {{{

type WorkerId = Int

type RemovalPriority = Word64

type InnerMonad inner_state = StateT inner_state IO

data WorkgroupCallbacks inner_state = WorkgroupCallbacks -- {{{
    {   createWorker :: WorkerId → InnerMonad inner_state ()
    ,   destroyWorker :: WorkerId → Bool → InnerMonad inner_state ()
    ,   killAllWorkers :: [WorkerId] → InnerMonad inner_state ()
    ,   sendProgressUpdateRequest :: WorkerId → InnerMonad inner_state ()
    ,   sendWorkloadStealRequest :: WorkerId → InnerMonad inner_state ()
    ,   sendWorkloadToWorker :: WorkerId → VisitorWorkload → InnerMonad inner_state ()
    }
-- }}}

data WorkgroupReceivers result = WorkgroupReceivers -- {{{
    {   receiveProgressUpdateFromWorker :: WorkerId → VisitorWorkerProgressUpdate result → IO ()
    ,   receiveStolenWorkloadFromWorker :: WorkerId → Maybe (VisitorWorkerStolenWorkload result) → IO ()
    ,   receiveWorkerTerminationReasonWithRemovalFlag :: WorkerId → (VisitorWorkerTerminationReason result) → IO ()
    }
-- }}}

data WorkgroupState result = WorkgroupState -- {{{
    {   pending_removal_ :: !IntSet
    ,   next_worker_id_ :: !WorkerId
    ,   next_priority_ :: !RemovalPriority
    ,   removal_queue_ :: !(PSQ WorkerId RemovalPriority)
    }
$( deriveAccessors ''WorkgroupState )
-- }}}

type WorkgroupStateMonad inner_state result = StateT (WorkgroupState result) (ReaderT (WorkgroupCallbacks inner_state) (InnerMonad inner_state))

type WorkgroupRequestQueue inner_state result = RequestQueue result WorkerId (WorkgroupStateMonad inner_state result)

type WorkgroupMonad inner_state result = VisitorSupervisorMonad result WorkerId (WorkgroupStateMonad inner_state result)

newtype WorkgroupControllerMonad inner_state result α = C { unwrapC :: RequestQueueReader result WorkerId (WorkgroupStateMonad inner_state result) α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO)
-- }}}

-- Classes {{{
class RequestQueueMonad m ⇒ WorkgroupRequestQueueMonad m where -- {{{
    changeNumberOfWorkersAsync :: (Int → IO Int) → (Int → IO ()) → m ()
-- }}}
-- }}}

-- Instances {{{
instance RequestQueueMonad (WorkgroupControllerMonad inner_state result) where -- {{{
    type RequestQueueMonadResult (WorkgroupControllerMonad inner_state result) = result
    abort = C abort
    fork = C . fork . unwrapC
    getCurrentProgressAsync = C . getCurrentProgressAsync
    getNumberOfWorkersAsync = C . getNumberOfWorkersAsync
    requestProgressUpdateAsync = C . requestProgressUpdateAsync
-- }}}
instance WorkgroupRequestQueueMonad (WorkgroupControllerMonad inner_state result) where -- {{{
    changeNumberOfWorkersAsync computeNewNumberOfWorkers receiveNewNumberOfWorkers = C $ ask >>= (enqueueRequest $ do
        old_number_of_workers ← numberOfWorkers
        new_number_of_workers ← liftIO $ computeNewNumberOfWorkers old_number_of_workers
        case new_number_of_workers `compare` old_number_of_workers of
            GT → replicateM_ (new_number_of_workers - old_number_of_workers) hireAWorker
            LT → replicateM_ (old_number_of_workers - new_number_of_workers) fireAWorker
            EQ → return ()
        liftIO . receiveNewNumberOfWorkers $ new_number_of_workers
     )
-- }}}
-- }}}

-- Exposed Functions {{{

changeNumberOfWorkers :: -- {{{
    WorkgroupRequestQueueMonad m ⇒
    (Int → IO Int) →
    m Int
changeNumberOfWorkers = syncAsync . changeNumberOfWorkersAsync
-- }}}

runWorkgroup :: -- {{{
    Monoid result ⇒
    inner_state →
    (WorkgroupReceivers result → WorkgroupCallbacks inner_state) →
    Maybe (VisitorProgress result) →
    WorkgroupControllerMonad inner_state result () →
    IO (TerminationReason result)
runWorkgroup initial_inner_state constructCallbacks maybe_starting_progress (C controller) = do
    request_queue ← newRequestQueue
    let receiveStolenWorkloadFromWorker = flip enqueueRequest request_queue .* receiveStolenWorkload
        receiveProgressUpdateFromWorker = flip enqueueRequest request_queue .* receiveProgressUpdate
        receiveWorkerTerminationReasonWithRemovalFlag worker_id termination_reason = flip enqueueRequest request_queue $ do
            removal_flag ← IntSet.member worker_id <$> getAndModify pending_removal (IntSet.delete worker_id)
            case termination_reason of
                VisitorWorkerFinished final_progress → do
                    infoM $ if removal_flag
                        then "Worker " ++ show worker_id ++ " has finished, and will be removed."
                        else "Worker " ++ show worker_id ++ " has finished, and will look for another workload."
                    receiveWorkerFinishedWithRemovalFlag removal_flag worker_id final_progress
                VisitorWorkerFailed message →
                    receiveWorkerFailure worker_id message
                VisitorWorkerAborted
                  | removal_flag → do
                     infoM $ "Worker " ++ show worker_id ++ " has been aborted."
                     removeWorker worker_id
                  | otherwise → error $ "Worker " ++ show worker_id ++ " aborted prematurely."
    manager_thread_id ← forkIO $ runReaderT controller request_queue
    termination_reason ←
        flip evalStateT initial_inner_state
        .
        flip runReaderT (constructCallbacks WorkgroupReceivers{..})
        .
        flip evalStateT initial_state
        $
        do  VisitorSupervisorResult termination_reason worker_ids ←
                runVisitorSupervisorMaybeStartingFrom
                    maybe_starting_progress
                    (constructWorkgroupActions request_queue)
                    $
                    -- enableSupervisorDebugMode >>
                    forever (processRequest request_queue)
            asks killAllWorkers >>= liftInner . ($ worker_ids)
            return $ case termination_reason of
                SupervisorAborted remaining_progress → Aborted remaining_progress
                SupervisorCompleted result → Completed result
                SupervisorFailure worker_id message →
                    Failure $ "Thread " ++ show worker_id ++ " failed with message: " ++ message
    killThread manager_thread_id
    return termination_reason
  where
    initial_state =
        WorkgroupState
            {   pending_removal_ = mempty
            ,   next_worker_id_ = 0
            ,   next_priority_ = maxBound
            ,   removal_queue_ = PSQ.empty
            }
-- }}}

-- }}}

-- Logging Functions {{{
logger_name = "Workgroup"

debugM, infoM :: MonadIO m ⇒ String → m ()
debugM = liftIO . Logger.debugM logger_name
infoM = liftIO . Logger.infoM logger_name
-- }}}

-- Internal Functions {{{

bumpWorkerRemovalPriority :: -- {{{
    (MonadState m, StateType m ~ WorkgroupState result) ⇒
    WorkerId →
    m ()
bumpWorkerRemovalPriority worker_id =
    getAndModify next_priority pred >>= (removal_queue %:) . PSQ.insert worker_id
-- }}}

constructWorkgroupActions :: -- {{{
    Monoid result ⇒
    WorkgroupRequestQueue inner_state  result →
    VisitorSupervisorActions result WorkerId (WorkgroupStateMonad inner_state result)
constructWorkgroupActions request_queue = VisitorSupervisorActions
    {   broadcast_progress_update_to_workers_action = \worker_ids →
            asks sendProgressUpdateRequest >>= liftInner . forM_ worker_ids
    ,   broadcast_workload_steal_to_workers_action = \worker_ids →
            asks sendWorkloadStealRequest >>= liftInner . forM_ worker_ids
    ,   receive_current_progress_action = receiveProgress request_queue
    ,   send_workload_to_worker_action = \workload worker_id → do
            infoM $ "Activating worker " ++ show worker_id ++ " with workload " ++ show workload
            asks sendWorkloadToWorker >>= liftInner . ($ workload) . ($ worker_id)
            bumpWorkerRemovalPriority worker_id
    }
-- }}}

fireAWorker :: -- {{{
    WorkgroupMonad inner_state result ()
fireAWorker =
    Set.minView <$> getWaitingWorkers
    >>= \x → case x of
        Just (worker_id,_) → do
            infoM $ "Removing waiting worker " ++ show worker_id ++ "."
            removeWorker worker_id
            removeWorkerFromRemovalQueue worker_id
            asks destroyWorker >>= liftInnerToSupervisor . ($ False) . ($ worker_id)
        Nothing → do
            (worker_id,new_removal_queue) ← do
                (PSQ.minView <$> get removal_queue) >>=
                    \x → case x of
                        Nothing → error "No workers found to be removed!"
                        Just (worker_id :-> _,rest_queue) → return (worker_id,rest_queue)
            infoM $ "Removing active worker " ++ show worker_id ++ "."
            removal_queue %= new_removal_queue
            pending_removal %: IntSet.insert worker_id
            asks destroyWorker >>= liftInnerToSupervisor . ($ True) . ($ worker_id)

-- }}}

hireAWorker :: -- {{{
    WorkgroupMonad inner_state result ()
hireAWorker = do
    worker_id ← getAndModify next_worker_id succ
    infoM $ "Adding worker " ++ show worker_id
    bumpWorkerRemovalPriority worker_id
    asks createWorker >>= liftInnerToSupervisor . ($ worker_id)
    addWorker worker_id
-- }}}

liftInner :: InnerMonad inner_state α → WorkgroupStateMonad inner_state result α -- {{{
liftInner = lift . lift
-- }}}

liftInnerToSupervisor :: InnerMonad inner_state α → WorkgroupMonad inner_state result α -- {{{
liftInnerToSupervisor = lift . liftInner
-- }}}

numberOfWorkers :: WorkgroupMonad inner_state result Int -- {{{
numberOfWorkers = PSQ.size <$> get removal_queue
-- }}}

removeWorkerFromRemovalQueue :: WorkerId → WorkgroupMonad inner_state result () -- {{{
removeWorkerFromRemovalQueue = (removal_queue %:) . PSQ.delete
-- }}}

-- }}}
