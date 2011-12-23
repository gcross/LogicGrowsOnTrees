-- @+leo-ver=5-thin
-- @+node:gcross.20110923164140.1278: * @file WorkerQueue.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923164140.1279: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.WorkerQueue where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923164140.1280: ** << Import needed modules >>
import Data.PSQueue (Binding(..),PSQ)
import qualified Data.PSQueue as PSQ
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923164140.1292: ** Types
-- @+node:gcross.20110923164140.1293: *3* VisitorWorkerQueue
data VisitorWorkerQueue id = VisitorWorkerQueue
    {   workerQueueNextPriority :: Int
    ,   workerQueue :: PSQ id Int
    }
-- @+node:gcross.20110923164140.1281: ** Functions
-- @+node:gcross.20110923164140.1294: *3* enqueueWorker
enqueueWorker :: Ord id ⇒ id → VisitorWorkerQueue id → VisitorWorkerQueue id
enqueueWorker worker_id (VisitorWorkerQueue next_priority queue) =
    VisitorWorkerQueue (next_priority+1) (PSQ.insert worker_id next_priority queue)
-- @+node:gcross.20110923164140.1295: *3* getAndRotateWorker
getAndRotateWorker :: Ord id ⇒ VisitorWorkerQueue id → Maybe (id,VisitorWorkerQueue id)
getAndRotateWorker worker_queue@(VisitorWorkerQueue _ queue) =
    fmap (\(worker_id :-> _) → (worker_id,enqueueWorker worker_id worker_queue))
    .
    PSQ.findMin
    $
    queue
-- @+node:gcross.20110923164140.1296: *3* deleteWorker
deleteWorker :: Ord id ⇒ id → VisitorWorkerQueue id → VisitorWorkerQueue id
deleteWorker worker_id worker_queue@(VisitorWorkerQueue _ queue) =
    worker_queue { workerQueue = PSQ.delete worker_id queue }
-- @-others
-- @-leo
