-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.Supervisor.RequestQueue where

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId,forkIO)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan,newTChanIO,readTChan,tryReadTChan,writeTChan)
import Control.Exception (BlockedIndefinitelyOnMVar(..),catch)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad (join,liftM,liftM2)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..),ask)

import Data.Composition ((.*))
import Data.IORef (IORef,atomicModifyIORef,newIORef)
import Data.Monoid (Monoid)
import Data.Typeable (Typeable)

import Control.Visitor.Checkpoint (Progress)
import qualified Control.Visitor.Parallel.Common.Supervisor as Supervisor
import Control.Visitor.Parallel.Common.Supervisor (SupervisorFullConstraint,SupervisorMonad,SupervisorProgram(..))
-- }}}

-- Classes {{{

class MonadCatchIO m ⇒ RequestQueueMonad m where -- {{{
    type RequestQueueMonadIntermediateProgress m :: *
    abort :: m ()
    fork :: m () → m ThreadId
    getCurrentProgressAsync :: (RequestQueueMonadIntermediateProgress m → IO ()) → m ()
    getNumberOfWorkersAsync :: (Int → IO ()) → m ()
    requestProgressUpdateAsync :: (RequestQueueMonadIntermediateProgress m → IO ()) → m ()
-- }}}

-- }}}

-- Types {{{

type Request r iv ip fv fp worker_id m = SupervisorMonad r iv ip fv fp worker_id m ()
data RequestQueue r iv ip fv fp worker_id m = RequestQueue -- {{{
    {   requests :: !(TChan (Request r iv ip fv fp worker_id m))
    ,   receivers :: !(IORef [ip → IO ()])
    }
-- }}}
type RequestQueueReader r iv ip fv fp worker_id m = ReaderT (RequestQueue r iv ip fv fp worker_id m) IO

-- }}}

-- Instances {{{

instance (SupervisorFullConstraint worker_id m, MonadCatchIO m) ⇒ RequestQueueMonad (RequestQueueReader r iv ip fv fp worker_id m) where -- {{{
    type RequestQueueMonadIntermediateProgress (RequestQueueReader r iv ip fv fp worker_id m) = ip
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
    (ip → IO ()) →
    RequestQueue r iv ip fv fp worker_id m →
    m' ()
addProgressReceiver receiver =
    liftIO
    .
    flip atomicModifyIORef ((receiver:) &&& const ())
    .
    receivers
-- }}}

tryDequeueRequest :: -- {{{
    MonadIO m' ⇒
    RequestQueue r iv ip fv fp worker_id m →
    m' (Maybe (Request r iv ip fv fp worker_id m))
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
    Request r iv ip fv fp worker_id m →
    RequestQueue r iv ip fv fp worker_id m →
    m' ()
enqueueRequest = flip $
    (liftIO . atomically)
    .*
    (writeTChan . requests)
-- }}}

getCurrentProgress :: RequestQueueMonad m ⇒ m (RequestQueueMonadIntermediateProgress m) -- {{{
getCurrentProgress = syncAsync getCurrentProgressAsync
-- }}}

getQuantityAsync :: -- {{{
    (MonadIO m', Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    SupervisorMonad r iv ip fv fp worker_id m α →
    (α → IO ()) →
    RequestQueue r iv ip fv fp worker_id m →
    m' ()
getQuantityAsync getQuantity receiveQuantity =
    enqueueRequest $ getQuantity >>= liftIO . receiveQuantity
-- }}}

getNumberOfWorkers :: RequestQueueMonad m ⇒ m Int -- {{{
getNumberOfWorkers = syncAsync getNumberOfWorkersAsync
-- }}}

newRequestQueue ::  -- {{{
    MonadIO m' ⇒
    m' (RequestQueue r iv ip fv fp worker_id m)
newRequestQueue = liftIO $ liftM2 RequestQueue newTChanIO (newIORef [])
-- }}}

processAllRequests :: -- {{{
    MonadIO m ⇒
    RequestQueue r iv ip fv fp worker_id m →
    SupervisorMonad r iv ip fv fp worker_id m ()
processAllRequests (RequestQueue requests _) = go
  where
    go =
        (liftIO . atomically . tryReadTChan) requests
        >>=
        maybe (return ()) (>> go)
-- }}}

processRequest :: -- {{{
    MonadIO m ⇒
    RequestQueue r iv ip fv fp worker_id m →
    SupervisorMonad r iv ip fv fp worker_id m ()
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
    RequestQueue r iv ip fv fp worker_id m →
    ip →
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

requestProgressUpdate :: RequestQueueMonad m ⇒ m (RequestQueueMonadIntermediateProgress m) -- {{{
requestProgressUpdate = syncAsync requestProgressUpdateAsync
-- }}}

requestQueueProgram :: -- {{{
    MonadIO m ⇒
    SupervisorMonad r iv ip fv fp worker_id m () →
    RequestQueue r iv ip fv fp worker_id m →
    SupervisorProgram r iv ip fv fp worker_id m
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
