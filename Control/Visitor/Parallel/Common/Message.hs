-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.Message where

-- Imports {{{

import Data.Derive.Serialize
import Data.DeriveTH
import Data.Serialize

import Control.Visitor.Checkpoint
import qualified Control.Visitor.Parallel.Common.Worker as Worker
import Control.Visitor.Utils.Handle
import Control.Visitor.Workload

import System.IO (Handle)
-- }}}

-- Types {{{
data MessageForSupervisor ip fp = -- {{{
    Failed String
  | Finished fp
  | ProgressUpdate (Worker.ProgressUpdate ip)
  | StolenWorkload (Maybe (Worker.StolenWorkload ip))
  | WorkerQuit
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForSupervisor)
-- }}}

data MessageForSupervisorReceivers worker_id ip fp = MessageForSupervisorReceivers -- {{{
    {   receiveProgressUpdateFromWorker :: worker_id → Worker.ProgressUpdate ip → IO ()
    ,   receiveStolenWorkloadFromWorker :: worker_id → Maybe (Worker.StolenWorkload ip) → IO ()
    ,   receiveFailureFromWorker :: worker_id → String → IO ()
    ,   receiveFinishedFromWorker :: worker_id → fp → IO ()
    ,   receiveQuitFromWorker :: worker_id → IO ()
    }
-- }}}

data MessageForWorker = -- {{{
    RequestProgressUpdate
  | RequestWorkloadSteal
  | StartWorkload Workload
  | QuitWorker
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForWorker)
-- }}}

receiveAndProcessMessagesFromWorker :: -- {{{
    MessageForSupervisorReceivers worker_id ip fp →
    IO (MessageForSupervisor ip fp) →
    worker_id →
    IO ()
receiveAndProcessMessagesFromWorker
    MessageForSupervisorReceivers{..}
    receiveMessage
    worker_id
    = receiveNextMessage
  where
    receiveNextMessage = receiveMessage >>= processMessage
    processMessage (Failed message) = do
        receiveFailureFromWorker worker_id message
        receiveNextMessage
    processMessage (Finished final_progress) = do
        receiveFinishedFromWorker worker_id final_progress
        receiveNextMessage
    processMessage (ProgressUpdate progress_update) = do
        receiveProgressUpdateFromWorker worker_id progress_update
        receiveNextMessage
    processMessage (StolenWorkload stolen_workload) = do
        receiveStolenWorkloadFromWorker worker_id stolen_workload
        receiveNextMessage
    processMessage WorkerQuit =
        receiveQuitFromWorker worker_id
-- }}}

receiveAndProcessMessagesFromWorkerUsingHandle :: -- {{{
    (Serialize ip, Serialize fp) ⇒
    MessageForSupervisorReceivers worker_id ip fp →
    Handle →
    worker_id →
    IO ()
receiveAndProcessMessagesFromWorkerUsingHandle receivers handle worker_id =
    receiveAndProcessMessagesFromWorker receivers (receive handle) worker_id
-- }}}

-- }}}
