{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module provides most of the common functionality needed to implement a
    adapter where the number of workers can be adjusted during the run.
 -}
module LogicGrowsOnTrees.Parallel.Common.Workgroup
    (
    -- * Type-classes
      WorkgroupRequestQueueMonad(..)
    -- * Types
    , InnerMonad
    , MessageForSupervisorReceivers(..)
    , WorkerId
    , WorkgroupCallbacks(..)
    , WorkgroupControllerMonad(..)
    -- * Functions
    -- ** Worker count adjustment
    , changeNumberOfWorkers
    , setNumberOfWorkersAsync
    , setNumberOfWorkers
    -- ** Runner
    , runWorkgroup
    ) where

import Control.Applicative (Applicative,(<$>))
import Control.Lens (makeLenses)
import Control.Lens.Getter (use)
import Control.Lens.Lens ((<<%=))
import Control.Lens.Setter ((.=),(%=))
import Control.Monad (forM_,replicateM_,void)
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
import Data.Word (Word,Word64)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(INFO))
import System.Log.Logger.TH

import Text.Printf (printf)

import LogicGrowsOnTrees.Parallel.Common.Message
import LogicGrowsOnTrees.Parallel.Common.RequestQueue
import LogicGrowsOnTrees.Parallel.Common.Supervisor
import LogicGrowsOnTrees.Parallel.Main (RunOutcomeFor,extractRunOutcomeFromSupervisorOutcome)
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Workload

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [INFO]

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{-| A 'WorkgroupRequestQueueMonad' is a 'RequestQueueMonad' but with the
    additional ability to change the number of workers in the system.
 -}
class RequestQueueMonad m ⇒ WorkgroupRequestQueueMonad m where
    {-| Change the number of workers;  the first argument is a map that computes
        the new number of workers given the old number of workers, and the
        second argument is a callback that will be invoked with the new number
        of workers.

        See 'changeNumberOfWorkers' for the synchronous version of this request.

        If you just want to set the number of workers to some fixed value, then
        see 'setNumberOfWorkers' / 'setNumberOfWorkersAsync'.
     -}
    changeNumberOfWorkersAsync :: (Word → Word) → (Word → IO ()) → m ()

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

{-| The type of worker ids used by this module (an alias for 'Int'). -}
type WorkerId = Int

type RemovalPriority = Word64

{-| This is the monad in which the adapter specific code is run. -}
type InnerMonad inner_state = StateT inner_state IO

{-| A set of callbacks invoked by the supervisor code in this module. -}
data WorkgroupCallbacks inner_state = WorkgroupCallbacks
    {   {-| create a worker with the given id -}
        createWorker :: WorkerId → InnerMonad inner_state ()
        {-| destroy the worker with the given id; ideally this should be
            implemented by signaling the worker to quit and then waiting for it
            to respond
         -}
    ,   destroyWorker :: WorkerId → Bool → InnerMonad inner_state ()
        {-| destroy all of the workers in the given list in a manner that
            ensures they all terminate promptly; this will be called at the end
            of the run (successful or not)
         -}
    ,   killAllWorkers :: [WorkerId] → InnerMonad inner_state ()
        {-| send a progress update request to the given worker -}
    ,   sendProgressUpdateRequestTo :: WorkerId → InnerMonad inner_state ()
        {-| send a workload steal request to the given worker -}
    ,   sendWorkloadStealRequestTo :: WorkerId → InnerMonad inner_state ()
        {-| send a workload to the given worker -}
    ,   sendWorkloadTo :: WorkerId → Workload → InnerMonad inner_state ()
    }

data WorkgroupState = WorkgroupState
    {   _pending_quit :: !IntSet
    ,   _next_worker_id :: !WorkerId
    ,   _next_priority :: !RemovalPriority
    ,   _removal_queue :: !(PSQ WorkerId RemovalPriority)
    }
$( makeLenses ''WorkgroupState )


type WorkgroupStateMonad inner_state = StateT WorkgroupState (ReaderT (WorkgroupCallbacks inner_state) (InnerMonad inner_state))

type WorkgroupMonad inner_state exploration_mode = SupervisorMonad exploration_mode WorkerId (WorkgroupStateMonad inner_state)

{-| This is the monad in which the workgroup controller will run. -}
newtype WorkgroupControllerMonad inner_state exploration_mode α = C { unwrapC :: RequestQueueReader exploration_mode WorkerId (WorkgroupStateMonad inner_state) α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad)

instance HasExplorationMode (WorkgroupControllerMonad inner_state exploration_mode) where
    type ExplorationModeFor (WorkgroupControllerMonad inner_state exploration_mode) = exploration_mode

instance WorkgroupRequestQueueMonad (WorkgroupControllerMonad inner_state exploration_mode) where
    changeNumberOfWorkersAsync computeNewNumberOfWorkers receiveNewNumberOfWorkers = C $ ask >>= (enqueueRequest $ do
        old_number_of_workers ← numberOfWorkers
        let new_number_of_workers = computeNewNumberOfWorkers old_number_of_workers
        case new_number_of_workers `compare` old_number_of_workers of
            GT → replicateM_ (fromIntegral $ new_number_of_workers - old_number_of_workers) hireAWorker
            LT → replicateM_ (fromIntegral $ old_number_of_workers - new_number_of_workers) fireAWorker
            EQ → return ()
        infoM $ printf "Number of workers has been changed from %i to %i" old_number_of_workers new_number_of_workers
        liftIO . receiveNewNumberOfWorkers $ new_number_of_workers
     )

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

--------------------------- Worker count adjustment ----------------------------

{-| Like 'changeNumberOfWorkersAsync', but it blocks until the number of workers
    has been changed and returns the new number of workers.
 -}
changeNumberOfWorkers :: WorkgroupRequestQueueMonad m ⇒ (Word → Word) → m Word
changeNumberOfWorkers = syncAsync . changeNumberOfWorkersAsync

{-| Request that the number of workers be set to the given amount, invoking the given callback when this has been done. -}
setNumberOfWorkersAsync :: WorkgroupRequestQueueMonad m ⇒ Word → IO () → m ()
setNumberOfWorkersAsync n callback = changeNumberOfWorkersAsync (const n) (const callback)

{-| Like 'setNumberOfWorkersAsync', but blocks until the number of workers has been set to the desired value. -}
setNumberOfWorkers :: WorkgroupRequestQueueMonad m ⇒ Word → m ()
setNumberOfWorkers = void . syncAsync . changeNumberOfWorkersAsync . const

{-| Explores a tree using a workgroup;  this function is only intended to be
    used by adapters where the number of workers can be changed on demand.
 -}
runWorkgroup ::
    ExplorationMode exploration_mode {-^ the mode in which we are exploring the tree -} →
    inner_state {-^ the initial adapter specific state of the inner monad -} →
    (MessageForSupervisorReceivers exploration_mode WorkerId → WorkgroupCallbacks inner_state)
        {-^ This function constructs a set of callbacks to be used by the
            supervisor loop in this function to do things like creating and
            destroying workers;  it is given a set of callbacks that allows the
            adapter specific code to signal conditions to the supervisor.
         -} →
    ProgressFor exploration_mode {-^ the initial progress of the exploration -} →
    WorkgroupControllerMonad inner_state exploration_mode () {-^ the controller, which is at the very least responsible for deciding how many workers should be initially created -} →
    IO (RunOutcomeFor exploration_mode)
runWorkgroup exploration_mode initial_inner_state constructCallbacks starting_progress (C controller) = do
    request_queue ← newRequestQueue
    let receiveStolenWorkloadFromWorker = flip enqueueRequest request_queue .* receiveStolenWorkload
        receiveProgressUpdateFromWorker = flip enqueueRequest request_queue .* receiveProgressUpdate
        receiveFailureFromWorker = flip enqueueRequest request_queue .* receiveWorkerFailure
        receiveFinishedFromWorker worker_id final_progress = flip enqueueRequest request_queue $ do
            removal_flag ← IntSet.member worker_id <$> use pending_quit
            infoM $ if removal_flag
                then "Worker " ++ show worker_id ++ " has finished, and will be removed."
                else "Worker " ++ show worker_id ++ " has finished, and will look for another workload."
            receiveWorkerFinishedWithRemovalFlag removal_flag worker_id final_progress

        receiveQuitFromWorker worker_id = flip enqueueRequest request_queue $ do
            infoM $ "Worker " ++ show worker_id ++ " has quit."
            quitting ← IntSet.member worker_id <$> (pending_quit <<%= IntSet.delete worker_id)
            if quitting
                then removeWorkerIfPresent worker_id
                else receiveWorkerFailure worker_id $ "Worker " ++ show worker_id ++ " quit prematurely."

        broadcastProgressUpdateToWorkers = \worker_ids →
            asks sendProgressUpdateRequestTo >>= liftInner . forM_ worker_ids
        broadcastWorkloadStealToWorkers = \worker_ids →
            asks sendWorkloadStealRequestTo >>= liftInner . forM_ worker_ids
        receiveCurrentProgress = receiveProgress request_queue
        sendWorkloadToWorker = \workload worker_id → do
            infoM $ "Activating worker " ++ show worker_id ++ " with workload " ++ show workload
            asks sendWorkloadTo >>= liftInner . ($ workload) . ($ worker_id)
            bumpWorkerRemovalPriority worker_id
    forkControllerThread request_queue controller
    run_outcome ←
        flip evalStateT initial_inner_state
        .
        flip runReaderT (constructCallbacks MessageForSupervisorReceivers{..})
        .
        flip evalStateT initial_state
        $
        do  supervisor_outcome@SupervisorOutcome{supervisorRemainingWorkers} ←
                runSupervisorStartingFrom
                    exploration_mode
                    starting_progress
                    SupervisorCallbacks{..}
                    (requestQueueProgram (return ()) request_queue)
            asks killAllWorkers >>= liftInner . ($ supervisorRemainingWorkers)
            return $ extractRunOutcomeFromSupervisorOutcome supervisor_outcome
    killControllerThreads request_queue
    return run_outcome
  where
    initial_state =
        WorkgroupState
            {   _pending_quit = mempty
            ,   _next_worker_id = 0
            ,   _next_priority = maxBound
            ,   _removal_queue = PSQ.empty
            }

--------------------------------------------------------------------------------
------------------------------ Internal Functions ------------------------------
--------------------------------------------------------------------------------

bumpWorkerRemovalPriority ::
    (MonadState WorkgroupState m) ⇒
    WorkerId →
    m ()
bumpWorkerRemovalPriority worker_id =
    (next_priority <<%= pred) >>= (removal_queue %=) . PSQ.insert worker_id

fireAWorker :: WorkgroupMonad inner_state exploration_mode ()
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

hireAWorker :: WorkgroupMonad inner_state exploration_mode ()
hireAWorker = do
    worker_id ← next_worker_id <<%= succ
    bumpWorkerRemovalPriority worker_id
    asks createWorker >>= liftInnerToSupervisor . ($ worker_id)
    addWorker worker_id

liftInner :: InnerMonad inner_state α → WorkgroupStateMonad inner_state α
liftInner = lift . lift

liftInnerToSupervisor :: InnerMonad inner_state α → WorkgroupMonad inner_state exploration_mode α
liftInnerToSupervisor = lift . liftInner

numberOfWorkers :: WorkgroupMonad inner_state exploration_mode Word
numberOfWorkers = fromIntegral . PSQ.size <$> use removal_queue

removeWorkerFromRemovalQueue :: WorkerId → WorkgroupMonad inner_state exploration_mode ()
removeWorkerFromRemovalQueue = (removal_queue %=) . PSQ.delete
