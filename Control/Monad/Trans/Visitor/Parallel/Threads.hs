-- Language extensions {{{
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Parallel.Threads where

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((&&&),(***))
import Control.Concurrent (forkIO,killThread,yield)
import Control.Concurrent.Chan (Chan,newChan,readChan,writeChan)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (BlockedIndefinitelyOnMVar(..),SomeException,assert,catch,evaluate)
import Control.Monad (forever,forM_,join,mapM_,replicateM_,unless,when)
import Control.Monad.CatchIO (MonadCatchIO,throw)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.State.Class (MonadState,StateType)
import Control.Monad.STM (atomically)
import Control.Monad.Tools (unlessM)
import Control.Monad.Trans.State.Strict (StateT,evalStateT)

import Data.Accessor.Monad.TF.State ((%=),(%:),get,getAndModify)
import Data.Accessor.Template (deriveAccessors)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef (atomicModifyIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.PSQueue (Binding((:->)),PSQ)
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set
import Data.Sequence ((|>))
import Data.Word

import qualified System.Log.Logger as Logger

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Supervisor
import qualified Control.Monad.Trans.Visitor.Supervisor as Supervisor
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Types {{{

type WorkerId = Int

type RemovalPriority = Word64

data WorkgroupState result = WorkgroupState -- {{{
    {   active_workers_ :: !(IntMap (VisitorWorkerEnvironment result))
    ,   next_worker_id_ :: !WorkerId
    ,   next_priority_ :: !RemovalPriority
    ,   progress_receivers_ :: ![VisitorProgress result → IO ()]
    ,   removal_queue_ :: !(PSQ WorkerId RemovalPriority)
    }
$( deriveAccessors ''WorkgroupState )
-- }}}

type WorkgroupStateMonad result = StateT (WorkgroupState result) IO

type WorkgroupMonad result = VisitorSupervisorMonad result WorkerId (WorkgroupStateMonad result)

type Controller result = Chan (WorkgroupMonad result ())

newtype WorkgroupController result = C { unwrapWorkgroupController :: Controller result }

data TerminationReason result = -- {{{
    Completed result
  | Aborted (VisitorProgress result)
  | Failure SomeException
-- }}}

-- }}}

-- Exposed Functions {{{

abort :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    IO ()
abort = flip writeChan abortSupervisor . unwrapWorkgroupController
-- }}}

changeNumberOfWorkers :: -- {{{
    Monoid result ⇒
    (Int → IO Int) →
    WorkgroupController result →
    IO Int
changeNumberOfWorkers = syncAsync . changeNumberOfWorkersAsync
-- }}}

changeNumberOfWorkersAsync :: -- {{{
    Monoid result ⇒
    (Int → IO Int) →
    WorkgroupController result →
    (Int → IO ()) →
    IO ()
changeNumberOfWorkersAsync computeNewNumberOfWorkers (C controller) receiveNewNumberOfWorkers = writeChan controller $ do
    old_number_of_workers ← numberOfWorkers
    new_number_of_workers ← liftIO $ computeNewNumberOfWorkers old_number_of_workers
    case new_number_of_workers `compare` old_number_of_workers of
        GT → replicateM_ (new_number_of_workers - old_number_of_workers) hireAWorker
        LT → replicateM_ (old_number_of_workers - new_number_of_workers) fireAWorker
        EQ → return ()
    liftIO . receiveNewNumberOfWorkers $ new_number_of_workers
-- }}}

getCurrentProgress :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    IO (VisitorProgress result)
getCurrentProgress = syncAsync getCurrentProgressAsync
-- }}}

getCurrentProgressAsync :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    (VisitorProgress result → IO ()) →
    IO ()
getCurrentProgressAsync = getQuantityAsync Supervisor.getCurrentProgress
-- }}}

getNumberOfWorkers :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    IO Int
getNumberOfWorkers = syncAsync getNumberOfWorkersAsync
-- }}}

getNumberOfWorkersAsync :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    (Int → IO ()) →
    IO ()
getNumberOfWorkersAsync = getQuantityAsync numberOfWorkers
-- }}}

requestProgressUpdate :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    IO (VisitorProgress result)
requestProgressUpdate = syncAsync $ requestProgressUpdateAsync
-- }}}

requestProgressUpdateAsync :: -- {{{
    Monoid result ⇒
    WorkgroupController result →
    (VisitorProgress result → IO ()) →
    IO ()
requestProgressUpdateAsync (C controller) receiveUpdatedProgress = writeChan controller $ do
    progress_receivers %: (receiveUpdatedProgress:)
    performGlobalProgressUpdate
-- }}}

runVisitorIO :: -- {{{
    Monoid result ⇒
    (TerminationReason result → IO ()) →
    VisitorIO result →
    IO (WorkgroupController result)
runVisitorIO = runVisitorIOStartingFrom mempty
-- }}}

runVisitorIOStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    (TerminationReason result → IO ()) →
    VisitorIO result →
    IO (WorkgroupController result)
runVisitorIOStartingFrom starting_progress notifyFinished =
    genericRunVisitorStartingFrom starting_progress notifyFinished
    .
    flip forkVisitorIOWorkerThread
-- }}}

runVisitorT :: -- {{{
    (Monoid result, Functor m, MonadCatchIO m) ⇒
    (∀ α. m α → IO α) →
    (TerminationReason result → IO ()) →
    VisitorT m result →
    IO (WorkgroupController result)
runVisitorT = runVisitorTStartingFrom mempty
-- }}}

runVisitorTStartingFrom :: -- {{{
    (Monoid result, Functor m, MonadCatchIO m) ⇒
    VisitorProgress result →
    (∀ α. m α → IO α) →
    (TerminationReason result → IO ()) →
    VisitorT m result →
    IO (WorkgroupController result)
runVisitorTStartingFrom starting_progress runMonad notifyFinished =
    genericRunVisitorStartingFrom starting_progress notifyFinished
    .
    flip (forkVisitorTWorkerThread runMonad)
-- }}}

runVisitor :: -- {{{
    Monoid result ⇒
    (TerminationReason result → IO ()) →
    Visitor result →
    IO (WorkgroupController result)
runVisitor = runVisitorStartingFrom mempty
-- }}}

runVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    (TerminationReason result → IO ()) →
    Visitor result →
    IO (WorkgroupController result)
runVisitorStartingFrom starting_progress notifyFinished =
    genericRunVisitorStartingFrom starting_progress notifyFinished
    .
    flip forkVisitorWorkerThread
-- }}}

-- }}}

-- Logging Functions {{{
logger_name = "Threads"

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
    Controller result →
    ((VisitorWorkerTerminationReason result → IO ()) → VisitorWorkload → IO (VisitorWorkerEnvironment result)) →
    VisitorSupervisorActions result WorkerId (WorkgroupStateMonad result)
constructWorkgroupActions messages spawnWorker = VisitorSupervisorActions
    {   broadcast_progress_update_to_workers_action = \worker_ids → do
            workers ← get active_workers
            liftIO . forM_ worker_ids $ \worker_id →
                case IntMap.lookup worker_id workers of
                    Nothing → return ()
                    Just VisitorWorkerEnvironment{workerPendingRequests} →
                        sendProgressUpdateRequest
                            workerPendingRequests
                            (writeChan messages . receiveProgressUpdate worker_id)
    ,   broadcast_workload_steal_to_workers_action = \worker_ids_and_counts → do
            workers ← get active_workers
            liftIO . forM_ worker_ids_and_counts $ \(worker_id,count) →
                case IntMap.lookup worker_id workers of
                    Nothing → return ()
                    Just VisitorWorkerEnvironment{workerPendingRequests} → do
                        debugM $ "Sending " ++ show count ++ " steal request(s) to " ++ show worker_id ++ "."
                        replicateM_ count $
                            sendWorkloadStealRequest
                                workerPendingRequests
                                (writeChan messages . receiveStolenWorkload worker_id)
    ,   receive_current_progress_action = \progress → do
            get progress_receivers >>= mapM_ (liftIO . ($ progress))
            progress_receivers %= []
    ,   send_workload_to_worker_action = \workload worker_id → do
            infoM $ "Spawning worker " ++ show worker_id ++ " with workload " ++ show workload
            environment ← liftIO $ spawnWorker (writeChan messages . receiveTerminationReason worker_id) workload
            active_workers %: IntMap.insert worker_id environment
            bumpWorkerRemovalPriority worker_id
    }
  where
    receiveTerminationReason worker_id (VisitorWorkerFinished final_progress) = do
        remove_worker ← IntMap.notMember worker_id <$> get active_workers
        infoM $ if remove_worker
            then "Worker " ++ show worker_id ++ " has finished, and will be removed."
            else "Worker " ++ show worker_id ++ " has finished, and will look for another workload."
        receiveWorkerFinishedWithRemovalFlag remove_worker worker_id final_progress
    receiveTerminationReason _ (VisitorWorkerFailed exception) =
        throw exception
    receiveTerminationReason worker_id VisitorWorkerAborted = do
        infoM $ "Worker " ++ show worker_id ++ " has been aborted."
        removeWorker worker_id
-- }}}

fireAWorker :: -- {{{
    Monoid result ⇒
    WorkgroupMonad result ()
fireAWorker =
    Set.minView <$> getWaitingWorkers
    >>= \x → case x of
        Just (worker_id,_) → do
            infoM $ "Removing waiting worker " ++ show worker_id ++ "."
            removeWorker worker_id
            removeWorkerFromRemovalQueue worker_id
        Nothing → do
            (worker_id,new_removal_queue) ← do
                (PSQ.minView <$> get removal_queue) >>=
                    \x → case x of
                        Nothing → error "No workers found to be removed!"
                        Just (worker_id :-> _,rest_queue) → return (worker_id,rest_queue)
            infoM $ "Removing active worker " ++ show worker_id ++ "."
            removal_queue %= new_removal_queue
            VisitorWorkerEnvironment{workerPendingRequests} ←
                fromMaybe (error $ "Active worker " ++ show worker_id ++ " not found in the map of active workers!")
                <$>
                IntMap.lookup worker_id
                <$>
                get active_workers
            active_workers %: IntMap.delete worker_id
            liftIO $ sendAbortRequest workerPendingRequests
-- }}}

genericRunVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    (TerminationReason result → IO ()) →
    ((VisitorWorkerTerminationReason result → IO ()) → VisitorWorkload → IO (VisitorWorkerEnvironment result)) →
    IO (WorkgroupController result)
genericRunVisitorStartingFrom starting_progress notifyFinished spawnWorker = do
    messages ← newChan
    forkIO $
        (flip catch (return . Failure) . flip evalStateT initial_state $ do
            VisitorSupervisorResult termination_reason _ ←
                runVisitorSupervisor (constructWorkgroupActions messages spawnWorker) $
                    -- enableSupervisorDebugMode >>
                    forever (join . liftIO . readChan $ messages)
            (IntMap.elems <$> get active_workers)
                >>= mapM_ (liftIO . killThread . workerThreadId)
            return $ case termination_reason of
                Left remaining_progress → Aborted remaining_progress
                Right result → Completed result
        ) >>= notifyFinished
    return $ C messages
  where
    initial_state =
        WorkgroupState
            {   active_workers_ = mempty
            ,   next_worker_id_ = 0
            ,   next_priority_ = maxBound
            ,   progress_receivers_ = []
            ,   removal_queue_ = PSQ.empty
            }
-- }}}

getQuantityAsync :: -- {{{
    Monoid result ⇒
    WorkgroupMonad result α →
    WorkgroupController result →
    (α → IO ()) →
    IO ()
getQuantityAsync getQuantity (C controller) receiveQuantity =
    writeChan controller $ getQuantity >>= liftIO . receiveQuantity
-- }}}

hireAWorker :: -- {{{
    Monoid result ⇒
    WorkgroupMonad result ()
hireAWorker = do
    worker_id ← getAndModify next_worker_id succ
    infoM $ "Adding worker " ++ show worker_id
    bumpWorkerRemovalPriority worker_id
    addWorker worker_id
-- }}}

numberOfWorkers :: WorkgroupMonad result Int -- {{{
numberOfWorkers = PSQ.size <$> get removal_queue
-- }}}

removeWorkerFromRemovalQueue :: WorkerId → WorkgroupMonad result () -- {{{
removeWorkerFromRemovalQueue = (removal_queue %:) . PSQ.delete
-- }}}

syncAsync :: -- {{{
    Monoid result ⇒
    (WorkgroupController result → (α → IO ()) → IO ()) →
    WorkgroupController result →
    IO α
syncAsync runCommandAsync controller = do
    result_mvar ← newEmptyMVar
    runCommandAsync controller (putMVar result_mvar)
    (takeMVar result_mvar)
     `catch`
      (\BlockedIndefinitelyOnMVar → error $ "blocked forever while waiting for controller to respond to request")
-- }}}

-- }}}
