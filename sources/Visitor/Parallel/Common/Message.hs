{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains infrastructure for communicating with workers over an
    inter-process channel.

    Note:  This module is used by the processes and network back-end, which are
           provided in separate packages.
 -}
module Visitor.Parallel.Common.Message
    (
    -- * Types
      MessageForSupervisor(..)
    , MessageForSupervisorFor(..)
    , MessageForSupervisorReceivers(..)
    , MessageForWorker(..)
    -- * Functions
    , receiveAndProcessMessagesFromWorker
    , receiveAndProcessMessagesFromWorkerUsingHandle
    ) where

import Data.Derive.Serialize
import Data.DeriveTH
import Data.Serialize

import Visitor.Parallel.Common.ExplorationMode
import qualified Visitor.Parallel.Common.Worker as Worker
import Visitor.Utils.Handle
import Visitor.Workload

import System.IO (Handle)

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

{-| A message from a worker to the supervisor;  the worker id is assumed to be
    known based on from where the message was received.
 -}
data MessageForSupervisor progress worker_final_progress =
    {-| The worker encountered a failure with the given message while visiting the tree. -}
    Failed String
    {-| The worker has finished with the given final progress. -}
  | Finished worker_final_progress
    {-| The worker has responded to the progress update request with the given progress update. -}
  | ProgressUpdate (Worker.ProgressUpdate progress)
    {-| The worker has responded to the workload steal request with possibly the stolen workload (and 'Nothing' if it was not possible to steal a workload at this time). -}
  | StolenWorkload (Maybe (Worker.StolenWorkload progress))
    {-| The worker has quit the system and is no longer available -}
  | WorkerQuit
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForSupervisor)

{-| Convenient type alias for obtaining the 'MessageForSupervisor' type corresponding with the given exploration mode. -}
type MessageForSupervisorFor exploration_mode = MessageForSupervisor (ProgressFor exploration_mode) (WorkerFinalProgressFor exploration_mode)

{-| This data structure contains callbacks to be invoked when a message has
    been received, depending on the kind of message.
 -}
data MessageForSupervisorReceivers exploration_mode worker_id = MessageForSupervisorReceivers
    {   {-| called when a progress update has been received from a worker -}
        receiveProgressUpdateFromWorker :: worker_id → Worker.ProgressUpdate (ProgressFor exploration_mode) → IO ()
        {-| called when a (possibly) stolen workload has been received from a worker -}
    ,   receiveStolenWorkloadFromWorker :: worker_id → Maybe (Worker.StolenWorkload (ProgressFor exploration_mode)) → IO ()
        {-| called when a failure (with the given message) has been received from a worker -}
    ,   receiveFailureFromWorker :: worker_id → String → IO ()
        {-| called when a worker has finished withthe given final progress -}
    ,   receiveFinishedFromWorker :: worker_id → WorkerFinalProgressFor exploration_mode → IO ()
        {-| called when a worker has quit the system and is no longer available -}
    ,   receiveQuitFromWorker :: worker_id → IO ()
    }

{-| A message from the supervisor to a worker.

    Note: It doesn't make sense to send, say, a progress update request when the
          worker is not processing a workload, nor does it make sense to send it
          a workload when it already has one.  It is your responsibility to not
          send a nonsense message.
 -}
data MessageForWorker =
    RequestProgressUpdate {-^ request a progress update -}
  | RequestWorkloadSteal {-^ request a stolen workload -}
  | StartWorkload Workload {-^ start visiting the given workload -}
  | QuitWorker {-^ stop what you are doing and quit the system -}
  deriving (Eq,Show)
$(derive makeSerialize ''MessageForWorker)

{-| This function continually performs an IO action to read a message from a
    worker with the given id and calls one of the given callbacks depending on
    the content of the message.
 -}
receiveAndProcessMessagesFromWorker ::
    MessageForSupervisorReceivers exploration_mode worker_id {-^ the callbacks to invoke when a message has been received -} →
    IO (MessageForSupervisorFor exploration_mode) {-^ an action that fetches the next message -} →
    worker_id {-^ the id of the worker from which messages are being received -} →
    IO () {-^ an IO action that continually processes incoming messages from a worker until it quits, at which point it returns -}
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

{-| This function is the same as 'receiveAndProcessMessagesFromWorker' except
    that instead of giving it an IO action to fetch a message you provide a
    'Handle' from which messsages (assumed to be deserializable) are read.
 -}
receiveAndProcessMessagesFromWorkerUsingHandle ::
    ( Serialize (ProgressFor exploration_mode)
    , Serialize (WorkerFinalProgressFor exploration_mode)
    ) ⇒
    MessageForSupervisorReceivers exploration_mode worker_id {-^ the callbacks to invoke when a message has been received -} →
    Handle {-^ the handle from which messages should be read -} →
    worker_id {-^ the id of the worker from which messages are being received -} →
    IO () {-^ an IO action that continually processes incoming messages from a worker until it quits, at which point it returns -}
receiveAndProcessMessagesFromWorkerUsingHandle receivers handle worker_id =
    receiveAndProcessMessagesFromWorker receivers (receive handle) worker_id



