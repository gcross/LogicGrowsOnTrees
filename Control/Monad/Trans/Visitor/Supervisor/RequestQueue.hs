-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Supervisor.RequestQueue -- {{{
    ( Request
    , RequestQueue
    , abort
    , addProgressReceiver
    , enqueueRequest
    , getCurrentProgress
    , getCurrentProgressAsync
    , getNumberOfWorkers
    , getNumberOfWorkersAsync
    , getQuantityAsync
    , newRequestQueue
    , processRequest
    , receiveProgress
    , requestProgressUpdate
    , requestProgressUpdateAsync
    , syncAsync
    ) where -- }}}

-- Imports {{{
import Prelude hiding (catch)

import Control.Arrow ((&&&))
import Control.Concurrent.Chan (Chan,newChan,readChan,writeChan)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (BlockedIndefinitelyOnMVar(..),catch)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad (join,liftM,liftM2)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Composition ((.*))
import Data.IORef (IORef,atomicModifyIORef,newIORef)
import Data.Monoid (Monoid)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor.Checkpoint (VisitorProgress)
import qualified Control.Monad.Trans.Visitor.Supervisor as Supervisor
import Control.Monad.Trans.Visitor.Supervisor (VisitorSupervisorMonad)
-- }}}

-- Types {{{
type Request result worker_id m = VisitorSupervisorMonad result worker_id m ()
data RequestQueue result worker_id m = RequestQueue -- {{{
    {   requests :: !(Chan (Request result worker_id m))
    ,   receivers :: !(IORef [VisitorProgress result → IO ()])
    }
-- }}}
-- }}}

-- Functions {{{

abort :: -- {{{
    (MonadIO m', Functor m, Monad m) ⇒
    RequestQueue result worker_id m →
    m' ()
abort = flip enqueueRequest Supervisor.abortSupervisor
-- }}}

addProgressReceiver :: -- {{{
    MonadIO m' ⇒
    RequestQueue result worker_id m →
    (VisitorProgress result → IO ()) →
    m' ()
addProgressReceiver queue receiver =
    liftIO
    .
    flip atomicModifyIORef ((receiver:) &&& const ())
    .
    receivers
    $
    queue
-- }}}

enqueueRequest :: -- {{{
    MonadIO m' ⇒
    RequestQueue result worker_id m →
    Request result worker_id m →
    m' ()
enqueueRequest =
    liftIO
    .*
    (writeChan . requests)
-- }}}

getCurrentProgress :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    RequestQueue result worker_id m →
    m' (VisitorProgress result)
getCurrentProgress = syncAsync getCurrentProgressAsync
-- }}}

getCurrentProgressAsync :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    RequestQueue result worker_id m →
    (VisitorProgress result → IO ()) →
    m' ()
getCurrentProgressAsync = getQuantityAsync Supervisor.getCurrentProgress
-- }}}

getQuantityAsync :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    VisitorSupervisorMonad result worker_id m α →
    RequestQueue result worker_id m →
    (α → IO ()) →
    m' ()
getQuantityAsync getQuantity request_queue receiveQuantity =
    enqueueRequest request_queue $ getQuantity >>= liftIO . receiveQuantity
-- }}}

getNumberOfWorkers :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    RequestQueue result worker_id m →
    m' Int
getNumberOfWorkers = syncAsync getNumberOfWorkersAsync
-- }}}

getNumberOfWorkersAsync :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    RequestQueue result worker_id m →
    (Int → IO ()) →
    m' ()
getNumberOfWorkersAsync = getQuantityAsync Supervisor.getNumberOfWorkers
-- }}}

newRequestQueue ::  -- {{{
    MonadIO m' ⇒
    m' (RequestQueue result worker_id m)
newRequestQueue = liftIO $ liftM2 RequestQueue newChan (newIORef [])
-- }}}

processRequest :: -- {{{
    MonadIO m ⇒
    RequestQueue result worker_id m →
    VisitorSupervisorMonad result worker_id m ()
processRequest =
    join
    .
    liftIO
    .
    readChan
    .
    requests
-- }}}

receiveProgress :: -- {{{
    MonadIO m' ⇒
    RequestQueue result worker_id m →
    VisitorProgress result →
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

requestProgressUpdate :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    RequestQueue result worker_id m →
    m' (VisitorProgress result)
requestProgressUpdate = syncAsync $ requestProgressUpdateAsync
-- }}}

requestProgressUpdateAsync :: -- {{{
    (MonadIO m', Monoid result, Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id, Functor m, MonadCatchIO m) ⇒
    RequestQueue result worker_id m →
    (VisitorProgress result → IO ()) →
    m' ()
requestProgressUpdateAsync queue receiveUpdatedProgress = liftIO $ do
    addProgressReceiver queue receiveUpdatedProgress
    enqueueRequest queue Supervisor.performGlobalProgressUpdate
-- }}}

syncAsync :: -- {{{
    MonadIO m' ⇒
    (RequestQueue result worker_id m → (α → IO ()) → IO ()) →
    RequestQueue result worker_id m →
    m' α
syncAsync runCommandAsync queue = liftIO $ do
    result_mvar ← newEmptyMVar
    runCommandAsync queue (putMVar result_mvar)
    (takeMVar result_mvar)
     `catch`
      (\BlockedIndefinitelyOnMVar → error $ "blocked forever while waiting for controller to respond to request")
-- }}}

-- }}}
