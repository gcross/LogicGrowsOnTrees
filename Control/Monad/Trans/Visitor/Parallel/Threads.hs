-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Parallel.Threads where

-- Imports {{{
import Control.Applicative (Applicative,(<$>))
import Control.Concurrent (forkIO,getNumCapabilities,killThread)
import Control.Monad (forever,forM_,mapM_,replicateM_,void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.State.Class (MonadState,StateType)
import Control.Monad.Trans.Reader (ask,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalStateT)

import Data.Accessor.Monad.TF.State ((%=),(%:),get,getAndModify)
import Data.Accessor.Template (deriveAccessors)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.PSQueue (Binding((:->)),PSQ)
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set
import Data.Word (Word64)

import qualified System.Log.Logger as Logger

import Control.Monad.Trans.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Supervisor
import Control.Monad.Trans.Visitor.Supervisor.Driver (Driver(Driver),TerminationReason(..))
import Control.Monad.Trans.Visitor.Supervisor.RequestQueue
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
    ,   removal_queue_ :: !(PSQ WorkerId RemovalPriority)
    }
$( deriveAccessors ''WorkgroupState )
-- }}}

type WorkgroupStateMonad result = StateT (WorkgroupState result) IO

type WorkgroupRequestQueue result = RequestQueue result WorkerId (WorkgroupStateMonad result)

type WorkgroupMonad result = VisitorSupervisorMonad result WorkerId (WorkgroupStateMonad result)

newtype WorkgroupControllerMonad result α = C { unwrapC :: RequestQueueReader result WorkerId (WorkgroupStateMonad result) α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO)
-- }}}

-- Instances {{{
instance RequestQueueMonad (WorkgroupControllerMonad result) where
    type RequestQueueMonadResult (WorkgroupControllerMonad result) = result
    abort = C abort
    fork = C . fork . unwrapC
    getCurrentProgressAsync = C . getCurrentProgressAsync
    getNumberOfWorkersAsync = C . getNumberOfWorkersAsync
    requestProgressUpdateAsync = C . requestProgressUpdateAsync
-- }}}

-- Driver {{{
driver :: Driver IO configuration result
driver = Driver
    (genericDriver runVisitorMaybeStartingFrom)
    (genericDriver runVisitorIOMaybeStartingFrom)
    (genericDriver . runVisitorTMaybeStartingFrom)
  where
    genericDriver run getConfiguration getMaybeStartingProgress notifyTerminated constructVisitor constructManager = do
        configuration ← getConfiguration
        maybe_starting_progress ← getMaybeStartingProgress configuration
        run  maybe_starting_progress
            (constructVisitor configuration)
            (changeNumberOfWorkersToMatchCPUs >> constructManager configuration)
         >>= notifyTerminated configuration
-- }}}

-- Exposed Functions {{{

changeNumberOfWorkers :: -- {{{
    (Int → IO Int) →
    WorkgroupControllerMonad result Int
changeNumberOfWorkers = syncAsync . changeNumberOfWorkersAsync
-- }}}

changeNumberOfWorkersAsync :: -- {{{
    (Int → IO Int) →
    (Int → IO ()) →
    WorkgroupControllerMonad result ()
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

changeNumberOfWorkersToMatchCPUs :: WorkgroupControllerMonad result () -- {{{
changeNumberOfWorkersToMatchCPUs =
    liftIO getNumCapabilities >>= \n → changeNumberOfWorkersAsync (const (return n)) (void . return)
-- }}}

runVisitor :: -- {{{
    Monoid result ⇒
    Visitor result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitor = runVisitorMaybeStartingFrom Nothing
-- }}}

runVisitorMaybeStartingFrom :: -- {{{
    Monoid result ⇒
    Maybe (VisitorProgress result) →
    Visitor result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorMaybeStartingFrom maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip forkVisitorWorkerThread
-- }}}

runVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    Visitor result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorStartingFrom = runVisitorMaybeStartingFrom . Just
-- }}}

runVisitorIO :: -- {{{
    Monoid result ⇒
    VisitorIO result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorIO = runVisitorIOMaybeStartingFrom Nothing
-- }}}

runVisitorIOMaybeStartingFrom :: -- {{{
    Monoid result ⇒
    Maybe (VisitorProgress result) →
    VisitorIO result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorIOMaybeStartingFrom maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip forkVisitorIOWorkerThread
-- }}}

runVisitorIOStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    VisitorIO result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorIOStartingFrom = runVisitorIOMaybeStartingFrom . Just
-- }}}

runVisitorT :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorT m result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorT = flip runVisitorTMaybeStartingFrom Nothing
-- }}}

runVisitorTMaybeStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    Maybe (VisitorProgress result) →
    VisitorT m result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorTMaybeStartingFrom runMonad maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip (forkVisitorTWorkerThread runMonad)
-- }}}

runVisitorTStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorProgress result →
    VisitorT m result →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
runVisitorTStartingFrom runInIO = runVisitorTMaybeStartingFrom runInIO . Just
-- }}}

-- }}}

-- Logging Functions {{{
logger_name = "Threads"

debugM, infoM :: MonadIO m ⇒ String → m ()
debugM = liftIO . Logger.debugM logger_name
infoM = liftIO . Logger.infoM logger_name
-- }}}

-- Internal Functions {{{

applyToSelectedActiveWorkers :: -- {{{
    Monoid result ⇒
    (WorkerId → VisitorWorkerEnvironment result → WorkgroupStateMonad result ()) →
    [WorkerId] →
    WorkgroupStateMonad result ()
applyToSelectedActiveWorkers action worker_ids = do
    workers ← get active_workers
    forM_ worker_ids $ \worker_id →
        maybe (return ()) (action worker_id) (IntMap.lookup worker_id workers)
-- }}}

bumpWorkerRemovalPriority :: -- {{{
    (MonadState m, StateType m ~ WorkgroupState result) ⇒
    WorkerId →
    m ()
bumpWorkerRemovalPriority worker_id =
    getAndModify next_priority pred >>= (removal_queue %:) . PSQ.insert worker_id
-- }}}

constructWorkgroupActions :: -- {{{
    Monoid result ⇒
    WorkgroupRequestQueue result →
    ((VisitorWorkerTerminationReason result → IO ()) → VisitorWorkload → IO (VisitorWorkerEnvironment result)) →
    VisitorSupervisorActions result WorkerId (WorkgroupStateMonad result)
constructWorkgroupActions request_queue spawnWorker = VisitorSupervisorActions
    {   broadcast_progress_update_to_workers_action =
            applyToSelectedActiveWorkers $ \worker_id (VisitorWorkerEnvironment{workerPendingRequests}) → liftIO $
                sendProgressUpdateRequest workerPendingRequests $ flip enqueueRequest request_queue . receiveProgressUpdate worker_id
    ,   broadcast_workload_steal_to_workers_action =
            applyToSelectedActiveWorkers $ \worker_id (VisitorWorkerEnvironment{workerPendingRequests}) → liftIO $
                sendWorkloadStealRequest workerPendingRequests $  flip enqueueRequest request_queue . receiveStolenWorkload worker_id
    ,   receive_current_progress_action = receiveProgress request_queue
    ,   send_workload_to_worker_action = \workload worker_id → do
            infoM $ "Spawning worker " ++ show worker_id ++ " with workload " ++ show workload
            environment ← liftIO $ spawnWorker (flip enqueueRequest request_queue . receiveTerminationReason worker_id) workload
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
    receiveTerminationReason worker_id (VisitorWorkerFailed exception) =
        receiveWorkerFailure worker_id (show exception)
    receiveTerminationReason worker_id VisitorWorkerAborted = do
        infoM $ "Worker " ++ show worker_id ++ " has been aborted."
        removeWorker worker_id
-- }}}

fireAWorker :: -- {{{
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
    Maybe (VisitorProgress result) →
    ((VisitorWorkerTerminationReason result → IO ()) → VisitorWorkload → IO (VisitorWorkerEnvironment result)) →
    WorkgroupControllerMonad result () →
    IO (TerminationReason result)
genericRunVisitorStartingFrom maybe_starting_progress spawnWorker (C controller) = do
    let starting_progress = fromMaybe mempty maybe_starting_progress
    request_queue ← newRequestQueue
    manager_thread_id ← forkIO $ runReaderT controller request_queue
    termination_reason ←
        flip evalStateT initial_state $ do
            VisitorSupervisorResult termination_reason _ ←
                runVisitorSupervisorStartingFrom
                    starting_progress
                    (constructWorkgroupActions request_queue spawnWorker)
                    $
                    -- enableSupervisorDebugMode >>
                    forever (processRequest request_queue)
            (IntMap.elems <$> get active_workers)
                >>= mapM_ (liftIO . killThread . workerThreadId)
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
            {   active_workers_ = mempty
            ,   next_worker_id_ = 0
            ,   next_priority_ = maxBound
            ,   removal_queue_ = PSQ.empty
            }
-- }}}

hireAWorker :: -- {{{
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

-- }}}
