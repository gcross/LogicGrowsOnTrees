-- Language extensions {{{
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Worker.Process where

-- Imports {{{
import Control.Concurrent (killThread)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class

import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor ((<$>))
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Maybe (isJust)
import Data.Monoid (Monoid)
import Data.Typeable (Typeable)
import Data.Serialize

import qualified System.Log.Logger as Logger

import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Supervisor
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Types {{{

data MessageForSupervisor result = -- {{{
    Failed String
  | Finished (VisitorProgress result)
  | ProgressUpdate (VisitorWorkerProgressUpdate result)
  | StolenWorkload (Maybe (VisitorWorkerStolenWorkload result))
  | WorkerQuit
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForSupervisor)
-- }}}

data MessageForWorker result = -- {{{
    RequestProgressUpdate
  | RequestWorkloadSteal
  | Workload VisitorWorkload
  | QuitWorker
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForWorker)
-- }}}

-- }}}

-- Logging Functions {{{
debugM :: String → IO ()
debugM = Logger.debugM "Worker"

infoM :: String → IO ()
infoM = Logger.infoM "Worker"
-- }}}

-- Functions {{{

runWorker :: -- {{{
    IO (MessageForWorker result) →
    (MessageForSupervisor result → IO ()) →
    (
        (VisitorWorkerTerminationReason result → IO ()) →
        VisitorWorkload →
        IO (VisitorWorkerEnvironment result)
    ) →
    IO ()
runWorker receiveMessage sendMessage forkWorkerThread =
    newIORef Nothing >>= \worker_environment →
    let processRequest sendRequest constructResponse =
            readIORef worker_environment
            >>=
            maybe (return ()) (flip sendRequest (sendMessage . constructResponse) . workerPendingRequests)
        processNextMessage = receiveMessage >>= \message →
            case message of
                RequestProgressUpdate → do
                    processRequest sendProgressUpdateRequest ProgressUpdate
                    processNextMessage
                RequestWorkloadSteal → do
                    processRequest sendWorkloadStealRequest StolenWorkload
                    processNextMessage
                Workload workload → do
                    infoM "Received workload."
                    debugM $ "Workload is: " ++ show workload
                    worker_is_running ← isJust <$> readIORef worker_environment
                    if worker_is_running
                        then sendMessage $ Failed "received a workload when the worker was already running"
                        else forkWorkerThread
                                (\termination_reason → do
                                    writeIORef worker_environment Nothing
                                    case termination_reason of
                                        VisitorWorkerFinished final_progress →
                                            sendMessage $ Finished final_progress
                                        VisitorWorkerFailed exception →
                                            sendMessage $ Failed (show exception)
                                        VisitorWorkerAborted →
                                            return ()
                                )
                                workload
                             >>=
                             writeIORef worker_environment . Just
                    processNextMessage
                QuitWorker → do
                    sendMessage WorkerQuit
                    liftIO $
                        readIORef worker_environment
                        >>=
                        maybe (return ()) (killThread . workerThreadId)
    in processNextMessage
-- }}}

-- }}}
