{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains functions that let one easily implement the worker side
    of an adapter under the assumption that the worker uses a two-way
    communication channel with the supervisor for sending and receiving
    messages.  (Examples of when this is NOT the case is the threads adapter,
    where you can communicate with the worker threads directly, and the MPI
    adapter, which has communication primitives that don't quite align with
    this setup.)
 -}
module LogicGrowsOnTrees.Parallel.Common.Process
    (
    -- * Exceptions
      ConnectionLost(..)
    -- * Functions
    , runWorker
    , runWorkerUsingHandles
    ) where

import Control.Concurrent (killThread)
import Control.Concurrent.MVar (isEmptyMVar,newEmptyMVar,newMVar,putMVar,takeMVar,tryTakeMVar,withMVar)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),Handler(..),catches,throwIO)
import Control.Monad.IO.Class

import Data.Functor ((<$>))
import Data.Serialize
import Data.Void (absurd)

import System.IO (Handle)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import LogicGrowsOnTrees (TreeT)
import LogicGrowsOnTrees.Parallel.Common.Message (MessageForSupervisor(..),MessageForSupervisorFor,MessageForWorker(..))
import LogicGrowsOnTrees.Parallel.Common.Worker hiding (ProgressUpdate,StolenWorkload)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ProgressFor,ResultFor,ExplorationMode(..),WorkerFinishedProgressFor)
import LogicGrowsOnTrees.Parallel.Purity
import LogicGrowsOnTrees.Utils.Handle

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG,INFO]

--------------------------------------------------------------------------------
----------------------------------- Functions ----------------------------------
--------------------------------------------------------------------------------

{-| Runs a loop that continually fetches and reacts to messages from the
    supervisor until the worker quits.
 -}
runWorker ::
    ∀ exploration_mode m n.
    ExplorationMode exploration_mode {-^ the mode in to explore the tree -} →
    Purity m n {-^ the purity of the tree -} →
    TreeT m (ResultFor exploration_mode) {-^ the tree -} →
    IO MessageForWorker {-^ the action used to fetch the next message -} →
    (MessageForSupervisorFor exploration_mode → IO ()) {-^ the action to send a message to the supervisor;  note that this might occur in a different thread from the worker loop -} →
    IO ()
runWorker exploration_mode purity tree receiveMessage sendMessage =
    -- Note:  This an MVar rather than an IORef because it is used by two
    --        threads --- this one and the worker thread --- and I wanted to use
    --        a mechanism that ensured that the new value would be observed by
    --        the other thread immediately rather than when the cache lines
    --        are flushed to the other processors.
    newEmptyMVar >>= \worker_environment_mvar →
    let processRequest ::
            (WorkerRequestQueue (ProgressFor exploration_mode) → (α → IO ()) → IO ()) →
            (α → MessageForSupervisorFor exploration_mode) →
            IO ()
        processRequest sendRequest constructResponse =
            tryTakeMVar worker_environment_mvar
            >>=
            maybe (return ()) (\worker_environment@WorkerEnvironment{workerPendingRequests} → do
                _ ← sendRequest workerPendingRequests (sendMessage . constructResponse)
                putMVar worker_environment_mvar worker_environment
            )
        processNextMessage = receiveMessage >>= \message →
            case message of
                RequestProgressUpdate → do
                    processRequest sendProgressUpdateRequest ProgressUpdate
                    processNextMessage
                RequestWorkloadSteal → do
                    processRequest sendWorkloadStealRequest StolenWorkload
                    processNextMessage
                StartWorkload workload → do
                    infoM "Received workload."
                    debugM $ "Workload is: " ++ show workload
                    worker_is_running ← not <$> isEmptyMVar worker_environment_mvar
                    if worker_is_running
                        then sendMessage $ Failed "received a workload when the worker was already running"
                        else forkWorkerThread
                                exploration_mode
                                purity
                                (\termination_reason → do
                                    _ ← takeMVar worker_environment_mvar
                                    case termination_reason of
                                        WorkerFinished final_progress →
                                            sendMessage $ Finished final_progress
                                        WorkerFailed exception →
                                            sendMessage $ Failed (show exception)
                                        WorkerAborted →
                                            return ()
                                )
                                tree
                                workload
                                (case exploration_mode of
                                    AllMode → absurd
                                    FirstMode → absurd
                                    FoundModeUsingPull _ → absurd
                                    FoundModeUsingPush _ → sendMessage . ProgressUpdate
                                )
                             >>=
                             putMVar worker_environment_mvar
                    processNextMessage
                QuitWorker → do
                    sendMessage WorkerQuit
                    liftIO $
                        tryTakeMVar worker_environment_mvar
                        >>=
                        maybe (return ()) (killThread . workerThreadId)
    in processNextMessage
        `catches`
        [Handler $ \e → case e of
            ThreadKilled → return ()
            UserInterrupt → return ()
            _ → throwIO e
        ,Handler $ \e → case e of
            ConnectionLost → debugM "Connection to supervisor was lost before this process had finished."
        ]
{-# INLINE runWorker #-}

{-| The same as 'runWorker', but it lets you provide handles through which the
    messages will be sent and received. (Note that the reading and writing
    handles might be the same.)
 -}
runWorkerUsingHandles ::
    ( Serialize (ProgressFor exploration_mode)
    , Serialize (WorkerFinishedProgressFor exploration_mode)
    ) ⇒
    ExplorationMode exploration_mode {-^ the mode in to explore the tree -} →
    Purity m n {-^ the purity of the tree -} →
    TreeT m (ResultFor exploration_mode) {-^ the tree -} →
    Handle {-^ handle from which messages from the supervisor are read -} →
    Handle {-^ handle to which messages to the supervisor are written -} →
    IO ()
runWorkerUsingHandles exploration_mode purity tree receive_handle send_handle =
    newMVar () >>= \send_lock →
    runWorker
        exploration_mode
        purity
        tree
        (receive receive_handle)
        (withMVar send_lock . const . send send_handle)
{-# INLINE runWorkerUsingHandles #-}
