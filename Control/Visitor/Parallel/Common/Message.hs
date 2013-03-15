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
import Control.Visitor.Workload
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

data MessageForSupervisorReceivers worker_id result = MessageForSupervisorReceivers -- {{{
    {   receiveProgressUpdateFromWorker :: worker_id → Worker.ProgressUpdate result → IO ()
    ,   receiveStolenWorkloadFromWorker :: worker_id → Maybe (Worker.StolenWorkload result) → IO ()
    ,   receiveFailureFromWorker :: worker_id → String → IO ()
    ,   receiveFinishedFromWorker :: worker_id → (Progress result) → IO ()
    ,   receiveQuitFromWorker :: worker_id → IO ()
    }
-- }}}

data MessageForWorker result = -- {{{
    RequestProgressUpdate
  | RequestWorkloadSteal
  | StartWorkload Workload
  | QuitWorker
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForWorker)
-- }}}

receiveAndProcessMessagesFromWorker ::
    MessageForSupervisorReceivers worker_id result →
    IO (MessageForSupervisor result) →
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
