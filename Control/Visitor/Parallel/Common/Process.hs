-- Language extensions {{{
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.Process
    ( runWorker
    , runWorkerUsingHandles
    ) where

-- Imports {{{
import Control.Concurrent (killThread)
import Control.Concurrent.MVar (isEmptyMVar,newEmptyMVar,putMVar,takeMVar,tryTakeMVar)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),catchJust)
import Control.Monad.IO.Class

import Data.Functor ((<$>))
import Data.Serialize

import System.IO (Handle)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor.Parallel.Common.Message (MessageForSupervisor(..),MessageForWorker(..))
import Control.Visitor.Parallel.Common.Worker hiding (ProgressUpdate,StolenWorkload)
import Control.Visitor.Utils.Handle
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Functions {{{

runWorker :: -- {{{
    IO MessageForWorker →
    (MessageForSupervisor ip fp → IO ()) →
    (
        (WorkerTerminationReason fp → IO ()) →
        Workload →
        IO (WorkerEnvironment ip)
    ) →
    IO ()
runWorker receiveMessage sendMessage forkVisitorWorkerThread =
    newEmptyMVar >>= \worker_environment_mvar →
    let processRequest sendRequest constructResponse =
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
                        else forkVisitorWorkerThread
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
                                workload
                             >>=
                             putMVar worker_environment_mvar
                    processNextMessage
                QuitWorker → do
                    sendMessage WorkerQuit
                    liftIO $
                        tryTakeMVar worker_environment_mvar
                        >>=
                        maybe (return ()) (killThread . workerThreadId)
    in catchJust
        (\e → case e of
            ThreadKilled → Just ()
            UserInterrupt → Just ()
            _ → Nothing
        )
        processNextMessage
        (const $ return ())
-- }}}

runWorkerUsingHandles :: -- {{{
    Serialize result ⇒
    Handle →
    Handle →
    (
        (WorkerTerminationReason result → IO ()) →
        Workload →
        IO (WorkerEnvironment result)
    ) →
    IO ()
runWorkerUsingHandles receive_handle send_handle =
    runWorker (receive receive_handle) (send send_handle)
-- }}}

-- }}}
