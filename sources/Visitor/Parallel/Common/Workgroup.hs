-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Parallel.Common.Workgroup -- {{{
    ( MessageForSupervisorReceivers(..)
    , WorkerId
    , WorkgroupCallbacks(..)
    , WorkgroupControllerMonad(..)
    , WorkgroupRequestQueueMonad(..)
    , changeNumberOfWorkers
    , runWorkgroup
    ) where -- }}}

-- Imports {{{
import Control.Applicative (Applicative,(<$>))
import Control.Concurrent (forkIO,killThread)
import Control.Lens (makeLenses)
import Control.Lens.Getter (use)
import Control.Lens.Lens ((<<%=))
import Control.Lens.Setter ((.=),(%=))
import Control.Monad (forM_,replicateM_)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT,ask,runReaderT)
import Control.Monad.Trans.State.Strict (StateT,evalStateT)

import Data.Composition ((.*))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid(mempty))
import Data.PSQueue (Binding((:->)),PSQ)
import qualified Data.PSQueue as PSQ
import Data.Word (Word64)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(INFO))
import System.Log.Logger.TH

import Visitor.Parallel.Main (RunOutcomeFor,extractRunOutcomeFromSupervisorOutcome)
import Visitor.Parallel.Common.Message
import Visitor.Parallel.Common.Supervisor
import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.VisitorMode
import Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [INFO]
-- }}}

-- Types {{{

type WorkerId = Int

type RemovalPriority = Word64

type InnerMonad inner_state = StateT inner_state IO

data WorkgroupCallbacks inner_state = WorkgroupCallbacks -- {{{
    {   createWorker :: WorkerId → InnerMonad inner_state ()
    ,   destroyWorker :: WorkerId → Bool → InnerMonad inner_state ()
    ,   killAllWorkers :: [WorkerId] → InnerMonad inner_state ()
    ,   sendProgressUpdateRequestTo :: WorkerId → InnerMonad inner_state ()
    ,   sendWorkloadStealRequestTo :: WorkerId → InnerMonad inner_state ()
    ,   sendWorkloadTo :: WorkerId → Workload → InnerMonad inner_state ()
    }
-- }}}

data WorkgroupState = WorkgroupState -- {{{
    {   _pending_quit :: !IntSet
    ,   _next_worker_id :: !WorkerId
    ,   _next_priority :: !RemovalPriority
    ,   _removal_queue :: !(PSQ WorkerId RemovalPriority)
    }
$( makeLenses ''WorkgroupState )
-- }}}

type WorkgroupStateMonad inner_state = StateT WorkgroupState (ReaderT (WorkgroupCallbacks inner_state) (InnerMonad inner_state))

type WorkgroupMonad inner_state visitor_mode = SupervisorMonad visitor_mode WorkerId (WorkgroupStateMonad inner_state)

newtype WorkgroupControllerMonad inner_state visitor_mode α = C { unwrapC :: RequestQueueReader visitor_mode WorkerId (WorkgroupStateMonad inner_state) α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad)
-- }}}

-- Classes {{{
class RequestQueueMonad m ⇒ WorkgroupRequestQueueMonad m where -- {{{
    changeNumberOfWorkersAsync :: (Int → IO Int) → (Int → IO ()) → m ()
-- }}}
-- }}}

-- Instances {{{
instance HasVisitorMode (WorkgroupControllerMonad inner_state visitor_mode) where -- {{{
    type VisitorModeFor (WorkgroupControllerMonad inner_state visitor_mode) = visitor_mode
-- }}}
instance WorkgroupRequestQueueMonad (WorkgroupControllerMonad inner_state visitor_mode) where -- {{{
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
    VisitorMode visitor_mode →
    inner_state →
    (MessageForSupervisorReceivers visitor_mode WorkerId → WorkgroupCallbacks inner_state) →
    ProgressFor visitor_mode →
    WorkgroupControllerMonad inner_state visitor_mode () →
    IO (RunOutcomeFor visitor_mode)
runWorkgroup visitor_mode initial_inner_state constructCallbacks starting_progress (C controller) = do
    request_queue ← newRequestQueue
    let receiveStolenWorkloadFromWorker = flip enqueueRequest request_queue .* receiveStolenWorkload
        receiveProgressUpdateFromWorker = flip enqueueRequest request_queue .* receiveProgressUpdate
        receiveFailureFromWorker = flip enqueueRequest request_queue .* receiveWorkerFailure
        receiveFinishedFromWorker worker_id final_progress = flip enqueueRequest request_queue $ do -- {{{
            removal_flag ← IntSet.member worker_id <$> use pending_quit
            infoM $ if removal_flag
                then "Worker " ++ show worker_id ++ " has finished, and will be removed."
                else "Worker " ++ show worker_id ++ " has finished, and will look for another workload."
            receiveWorkerFinishedWithRemovalFlag removal_flag worker_id final_progress
        -- }}}
        receiveQuitFromWorker worker_id = flip enqueueRequest request_queue $ do -- {{{
            infoM $ "Worker " ++ show worker_id ++ " has quit."
            quitting ← IntSet.member worker_id <$> (pending_quit <<%= IntSet.delete worker_id)
            if quitting
                then removeWorkerIfPresent worker_id
                else receiveWorkerFailure worker_id $ "Worker " ++ show worker_id ++ " quit prematurely."
        -- }}}
        broadcastProgressUpdateToWorkers = \worker_ids →
            asks sendProgressUpdateRequestTo >>= liftInner . forM_ worker_ids
        broadcastWorkloadStealToWorkers = \worker_ids →
            asks sendWorkloadStealRequestTo >>= liftInner . forM_ worker_ids
        receiveCurrentProgress = receiveProgress request_queue
        sendWorkloadToWorker = \workload worker_id → do
            infoM $ "Activating worker " ++ show worker_id ++ " with workload " ++ show workload
            asks sendWorkloadTo >>= liftInner . ($ workload) . ($ worker_id)
            bumpWorkerRemovalPriority worker_id
    manager_thread_id ← forkIO $ runReaderT controller request_queue
    run_outcome ←
        flip evalStateT initial_inner_state
        .
        flip runReaderT (constructCallbacks MessageForSupervisorReceivers{..})
        .
        flip evalStateT initial_state
        $
        do  supervisor_outcome@SupervisorOutcome{supervisorRemainingWorkers} ←
                runSupervisorStartingFrom
                    visitor_mode
                    starting_progress
                    SupervisorCallbacks{..}
                    (requestQueueProgram (return ()) request_queue)
            asks killAllWorkers >>= liftInner . ($ supervisorRemainingWorkers)
            return $ extractRunOutcomeFromSupervisorOutcome supervisor_outcome
    killThread manager_thread_id
    return run_outcome
  where
    initial_state =
        WorkgroupState
            {   _pending_quit = mempty
            ,   _next_worker_id = 0
            ,   _next_priority = maxBound
            ,   _removal_queue = PSQ.empty
            }
-- }}}

-- }}}

-- Internal Functions {{{

bumpWorkerRemovalPriority :: -- {{{
    (MonadState WorkgroupState m) ⇒
    WorkerId →
    m ()
bumpWorkerRemovalPriority worker_id =
    (next_priority <<%= pred) >>= (removal_queue %=) . PSQ.insert worker_id
-- }}}

fireAWorker :: WorkgroupMonad inner_state visitor_mode () -- {{{
fireAWorker =
    tryGetWaitingWorker
    >>= \x → case x of
        Just worker_id → do
            infoM $ "Removing waiting worker " ++ show worker_id ++ "."
            removeWorker worker_id
            removeWorkerFromRemovalQueue worker_id
            pending_quit %= IntSet.insert worker_id
            asks destroyWorker >>= liftInnerToSupervisor . ($ False) . ($ worker_id)
        Nothing → do
            (worker_id,new_removal_queue) ← do
               (PSQ.minView <$> use removal_queue) >>=
                    \x → case x of
                        Nothing → error "No workers found to be removed!"
                        Just (worker_id :-> _,rest_queue) → return (worker_id,rest_queue)
            infoM $ "Removing active worker " ++ show worker_id ++ "."
            removal_queue .= new_removal_queue
            pending_quit %= IntSet.insert worker_id
            asks destroyWorker >>= liftInnerToSupervisor . ($ True) . ($ worker_id)

-- }}}

hireAWorker :: WorkgroupMonad inner_state visitor_mode () -- {{{
hireAWorker = do
    worker_id ← next_worker_id <<%= succ
    bumpWorkerRemovalPriority worker_id
    asks createWorker >>= liftInnerToSupervisor . ($ worker_id)
    addWorker worker_id
-- }}}

liftInner :: InnerMonad inner_state α → WorkgroupStateMonad inner_state α -- {{{
liftInner = lift . lift
-- }}}

liftInnerToSupervisor :: InnerMonad inner_state α → WorkgroupMonad inner_state visitor_mode α -- {{{
liftInnerToSupervisor = lift . liftInner
-- }}}

numberOfWorkers :: WorkgroupMonad inner_state visitor_mode Int -- {{{
numberOfWorkers = PSQ.size <$> use removal_queue
-- }}}

removeWorkerFromRemovalQueue :: WorkerId → WorkgroupMonad inner_state visitor_mode () -- {{{
removeWorkerFromRemovalQueue = (removal_queue %=) . PSQ.delete
-- }}}

-- }}}
