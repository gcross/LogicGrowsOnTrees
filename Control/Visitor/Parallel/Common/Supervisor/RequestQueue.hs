-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.Supervisor.RequestQueue
    ( Request
    , RequestQueue
    , RequestQueueMonad(..)
    , RequestQueueReader
    , addProgressReceiver
    , constructMessageReceiversWithPendingQuit
    , enqueueRequest
    , getCurrentProgress
    , getQuantityAsync
    , getNumberOfWorkers
    , newRequestQueue
    , processAllRequests
    , processRequest
    , receiveProgress
    , requestProgressUpdate
    , requestQueueProgram
    , syncAsync
    , tryDequeueRequest
    ) where

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId,forkIO)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan,newTChanIO,readTChan,tryReadTChan,writeTChan)
import Control.Exception (BlockedIndefinitelyOnMVar(..),catch)
import Control.Lens ((<<%=),Lens',use)
import Control.Monad (join,liftM,liftM2,when)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Reader (ReaderT(..),ask)

import Data.Composition ((.*))
import Data.Functor ((<$>))
import Data.IORef (IORef,atomicModifyIORef,newIORef)
import Data.Monoid (Monoid)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable (Typeable)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor.Checkpoint (Progress)
import Control.Visitor.Parallel.Common.Message
import qualified Control.Visitor.Parallel.Common.Supervisor as Supervisor
import Control.Visitor.Parallel.Common.Supervisor (SupervisorFullConstraint,SupervisorMonad,SupervisorProgram(..))
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Classes {{{

class MonadCatchIO m ⇒ RequestQueueMonad m where -- {{{
    type RequestQueueMonadResult m :: *
    abort :: m ()
    fork :: m () → m ThreadId
    getCurrentProgressAsync :: (Progress (RequestQueueMonadResult m) → IO ()) → m ()
    getNumberOfWorkersAsync :: (Int → IO ()) → m ()
    requestProgressUpdateAsync :: (Progress (RequestQueueMonadResult m) → IO ()) → m ()
-- }}}

-- }}}

-- Types {{{

type Request result worker_id m = SupervisorMonad result worker_id m ()
data RequestQueue result worker_id m = RequestQueue -- {{{
    {   requests :: !(TChan (Request result worker_id m))
    ,   receivers :: !(IORef [Progress result → IO ()])
    }
-- }}}
type RequestQueueReader result worker_id m = ReaderT (RequestQueue result worker_id m) IO

-- }}}

-- Instances {{{

instance (SupervisorFullConstraint worker_id m, MonadCatchIO m) ⇒ RequestQueueMonad (RequestQueueReader result worker_id m) where -- {{{
    type RequestQueueMonadResult (RequestQueueReader result worker_id m) = result
    abort = ask >>= enqueueRequest Supervisor.abortSupervisor
    fork m = ask >>= liftIO . forkIO . runReaderT m
    getCurrentProgressAsync = (ask >>=) . getQuantityAsync Supervisor.getCurrentProgress
    getNumberOfWorkersAsync = (ask >>=) . getQuantityAsync Supervisor.getNumberOfWorkers
    requestProgressUpdateAsync receiveUpdatedProgress = -- {{{
        ask
        >>=
        liftIO
        .
        liftA2 (>>)
            (addProgressReceiver receiveUpdatedProgress)
            (enqueueRequest Supervisor.performGlobalProgressUpdate)
    -- }}}
-- }}}

-- }}}

-- Functions {{{

addProgressReceiver :: -- {{{
    MonadIO m' ⇒
    (Progress result → IO ()) →
    RequestQueue result worker_id m →
    m' ()
addProgressReceiver receiver =
    liftIO
    .
    flip atomicModifyIORef ((receiver:) &&& const ())
    .
    receivers
-- }}}

constructMessageReceiversWithPendingQuit :: -- {{{
    ( Monoid result
    , SupervisorFullConstraint worker_id m
    , MonadState s m
    ) ⇒
    Bool →
    Lens' s (Set worker_id) →
    RequestQueue result worker_id m →
    MessageForSupervisorReceivers worker_id result
constructMessageReceiversWithPendingQuit fail_on_unexpected_quit pending_quit request_queue =
    MessageForSupervisorReceivers{..}
  where
    receiveProgressUpdateFromWorker = flip enqueueRequest request_queue .* Supervisor.receiveProgressUpdate
    receiveStolenWorkloadFromWorker = flip enqueueRequest request_queue .* Supervisor.receiveStolenWorkload
    receiveFailureFromWorker = flip enqueueRequest request_queue .* Supervisor.receiveWorkerFailure
    receiveFinishedFromWorker worker_id final_progress = flip enqueueRequest request_queue $ do -- {{{
        removal_flag ← Set.member worker_id <$> use pending_quit
        infoM $ if removal_flag
            then "Worker " ++ show worker_id ++ " has finished, and will be removed."
            else "Worker " ++ show worker_id ++ " has finished, and will look for another workload."
        Supervisor.receiveWorkerFinishedWithRemovalFlag removal_flag worker_id final_progress
    -- }}}
    receiveQuitFromWorker worker_id = flip enqueueRequest request_queue $ do -- {{{
        infoM $ "Worker " ++ show worker_id ++ " has quit."
        is_pending_quit ← Set.member worker_id <$> (pending_quit <<%= Set.delete worker_id)
        if not is_pending_quit && fail_on_unexpected_quit
            then Supervisor.receiveWorkerFailure worker_id $ "Worker " ++ show worker_id ++ " quit prematurely."
            else Supervisor.removeWorkerIfPresent worker_id
    -- }}}
-- }}}

tryDequeueRequest :: -- {{{
    MonadIO m' ⇒
    RequestQueue result worker_id m →
    m' (Maybe (Request result worker_id m))
tryDequeueRequest =
    liftIO
    .
    atomically
    .
    tryReadTChan
    .
    requests
-- }}}

enqueueRequest :: -- {{{
    MonadIO m' ⇒
    Request result worker_id m →
    RequestQueue result worker_id m →
    m' ()
enqueueRequest = flip $
    (liftIO . atomically)
    .*
    (writeTChan . requests)
-- }}}

getCurrentProgress :: RequestQueueMonad m ⇒ m (Progress (RequestQueueMonadResult m)) -- {{{
getCurrentProgress = syncAsync getCurrentProgressAsync
-- }}}

getQuantityAsync :: -- {{{
    (MonadIO m', Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    SupervisorMonad result worker_id m α →
    (α → IO ()) →
    RequestQueue result worker_id m →
    m' ()
getQuantityAsync getQuantity receiveQuantity =
    enqueueRequest $ getQuantity >>= liftIO . receiveQuantity
-- }}}

getNumberOfWorkers :: RequestQueueMonad m ⇒ m Int -- {{{
getNumberOfWorkers = syncAsync getNumberOfWorkersAsync
-- }}}

newRequestQueue ::  -- {{{
    MonadIO m' ⇒
    m' (RequestQueue result worker_id m)
newRequestQueue = liftIO $ liftM2 RequestQueue newTChanIO (newIORef [])
-- }}}

processAllRequests :: -- {{{
    MonadIO m ⇒
    RequestQueue result worker_id m →
    SupervisorMonad result worker_id m ()
processAllRequests (RequestQueue requests _) = go
  where
    go =
        (liftIO . atomically . tryReadTChan) requests
        >>=
        maybe (return ()) (>> go)
-- }}}

processRequest :: -- {{{
    MonadIO m ⇒
    RequestQueue result worker_id m →
    SupervisorMonad result worker_id m ()
processRequest =
    join
    .
    liftIO
    .
    atomically
    .
    readTChan
    .
    requests
-- }}}

receiveProgress :: -- {{{
    MonadIO m' ⇒
    RequestQueue result worker_id m →
    Progress result →
    m' ()
receiveProgress queue progress =
    liftIO
    .
    join
    .
    liftM (sequence_ . map ($ progress))
    .
    flip atomicModifyIORef (const [] &&& id)
    .
    receivers
    $
    queue
-- }}}

requestProgressUpdate :: RequestQueueMonad m ⇒ m (Progress (RequestQueueMonadResult m)) -- {{{
requestProgressUpdate = syncAsync requestProgressUpdateAsync
-- }}}

requestQueueProgram :: -- {{{
    MonadIO m ⇒
    SupervisorMonad result worker_id m () →
    RequestQueue result worker_id m →
    SupervisorProgram result worker_id m
requestQueueProgram initialize =
    flip (BlockingProgram initialize) id
    .
    liftIO
    .
    atomically
    .
    readTChan
    .
    requests
-- }}}

syncAsync :: MonadIO m ⇒ ((α → IO ()) → m ()) → m α -- {{{
syncAsync runCommandAsync = do
    result_mvar ← liftIO newEmptyMVar
    runCommandAsync (putMVar result_mvar)
    liftIO $
        takeMVar result_mvar
        `catch`
        (\BlockedIndefinitelyOnMVar → error $ "blocked forever while waiting for controller to respond to request")
-- }}}

-- }}}
