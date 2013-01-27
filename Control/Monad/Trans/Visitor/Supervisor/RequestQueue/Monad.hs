-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Supervisor.RequestQueue.Monad where

-- Imports {{{
import Prelude hiding (catch)

import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (BlockedIndefinitelyOnMVar(..),catch)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid (Monoid)

import Control.Monad.Trans.Visitor.Checkpoint (VisitorProgress)
-- }}}

-- Classes {{{
class (Monoid (RequestQueueMonadResult m), MonadCatchIO m) ⇒ RequestQueueMonad m where
    type RequestQueueMonadResult m :: *
    abort :: m ()
    getCurrentProgressAsync :: (VisitorProgress (RequestQueueMonadResult m) → IO ()) → m ()
    getNumberOfWorkersAsync :: (Int → IO ()) → m ()
    requestProgressUpdateAsync :: (VisitorProgress (RequestQueueMonadResult m) → IO ()) → m ()
-- }}}

-- Functions {{{

getCurrentProgress :: RequestQueueMonad m ⇒ m (VisitorProgress (RequestQueueMonadResult m)) -- {{{
getCurrentProgress = syncAsync getCurrentProgressAsync
-- }}}

getNumberOfWorkers :: RequestQueueMonad m ⇒ m Int -- {{{
getNumberOfWorkers = syncAsync getNumberOfWorkersAsync
-- }}}

requestProgressUpdate :: RequestQueueMonad m ⇒ m (VisitorProgress (RequestQueueMonadResult m)) -- {{{
requestProgressUpdate = syncAsync requestProgressUpdateAsync
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
