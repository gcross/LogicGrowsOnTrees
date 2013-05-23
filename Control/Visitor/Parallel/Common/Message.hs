-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Visitor.Parallel.Common.VisitorMode
import qualified Control.Visitor.Parallel.Common.Worker as Worker
import Control.Visitor.Utils.Handle
import Control.Visitor.Workload

import System.IO (Handle)
-- }}}

-- Types {{{
data MessageForSupervisor progress worker_final_progress = -- {{{
    Failed String
  | Finished worker_final_progress
  | ProgressUpdate (Worker.ProgressUpdate progress)
  | StolenWorkload (Maybe (Worker.StolenWorkload progress))
  | WorkerQuit
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForSupervisor)
-- }}}
type MessageForSupervisorForMode visitor_mode = MessageForSupervisor (ProgressFor visitor_mode) (WorkerFinalProgressFor visitor_mode)

data MessageForSupervisorReceivers visitor_mode worker_id = MessageForSupervisorReceivers -- {{{
    {   receiveProgressUpdateFromWorker :: worker_id → Worker.ProgressUpdate (ProgressFor visitor_mode) → IO ()
    ,   receiveStolenWorkloadFromWorker :: worker_id → Maybe (Worker.StolenWorkload (ProgressFor visitor_mode)) → IO ()
    ,   receiveFailureFromWorker :: worker_id → String → IO ()
    ,   receiveFinishedFromWorker :: worker_id → WorkerFinalProgressFor visitor_mode → IO ()
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
    MessageForSupervisorReceivers visitor_mode worker_id →
    IO (MessageForSupervisorForMode visitor_mode) →
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
    ( Serialize (ProgressFor visitor_mode)
    , Serialize (WorkerFinalProgressFor visitor_mode)
    ) ⇒
    MessageForSupervisorReceivers visitor_mode worker_id →
    Handle →
    worker_id →
    IO ()
receiveAndProcessMessagesFromWorkerUsingHandle receivers handle worker_id =
    receiveAndProcessMessagesFromWorker receivers (receive handle) worker_id
-- }}}

-- }}}
