-- Language extensions {{{
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Process
    ( MessageForSupervisor(..)
    , MessageForWorker(..)
    , runWorker
    ) where

-- Imports {{{
import Control.Concurrent (killThread)
import Control.Concurrent.MVar (isEmptyMVar,newEmptyMVar,putMVar,takeMVar,tryTakeMVar)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),catchJust)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class

import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor ((<$>))
import Data.Monoid (Monoid)
import Data.Typeable (Typeable)
import Data.Serialize

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor.Checkpoint
import Control.Visitor.Supervisor
import qualified Control.Visitor.Worker as Worker
import Control.Visitor.Worker hiding (ProgressUpdate,StolenWorkload)
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Types {{{

data MessageForSupervisor result = -- {{{
    Failed String
  | Finished (Progress result)
  | ProgressUpdate (Worker.ProgressUpdate result)
  | StolenWorkload (Maybe (Worker.StolenWorkload result))
  | WorkerQuit
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForSupervisor)
-- }}}

data MessageForWorker result = -- {{{
    RequestProgressUpdate
  | RequestWorkloadSteal
  | StartWorkload Workload
  | QuitWorker
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForWorker)
-- }}}

-- }}}

-- Functions {{{

runWorker :: -- {{{
    IO (MessageForWorker result) →
    (MessageForSupervisor result → IO ()) →
    (
        (WorkerTerminationReason result → IO ()) →
        Workload →
        IO (WorkerEnvironment result)
    ) →
    IO ()
runWorker receiveMessage sendMessage forkVisitorWorkerThread =
    newEmptyMVar >>= \worker_environment_mvar →
    let processRequest sendRequest constructResponse =
            tryTakeMVar worker_environment_mvar
            >>=
            maybe (return ()) (\worker_environment@WorkerEnvironment{workerPendingRequests} → do
                sendRequest workerPendingRequests (sendMessage . constructResponse)
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

-- }}}
