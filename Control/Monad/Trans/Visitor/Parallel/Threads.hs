-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Parallel.Threads where

-- Imports {{{
import Control.Applicative (Applicative)
import Control.Concurrent (forkIO,getNumCapabilities,killThread)
import Control.Monad (void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))

import Options.Applicative (InfoMod,execParser,info)

import qualified System.Log.Logger as Logger

import Control.Monad.Trans.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Parallel.Workgroup
import Control.Monad.Trans.Visitor.Supervisor.Driver (Driver(Driver),TerminationReason)
import Control.Monad.Trans.Visitor.Supervisor.RequestQueue
import Control.Monad.Trans.Visitor.Worker as Worker
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Types {{{
newtype ThreadsControllerMonad result α = C { unwrapC :: WorkgroupControllerMonad (IntMap (VisitorWorkerEnvironment result)) result α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,WorkgroupRequestQueueMonad)
-- }}}

-- Instances {{{
instance RequestQueueMonad (ThreadsControllerMonad result) where
    type RequestQueueMonadResult (ThreadsControllerMonad result) = result
    abort = C abort
    fork = C . fork . unwrapC
    getCurrentProgressAsync = C . getCurrentProgressAsync
    getNumberOfWorkersAsync = C . getNumberOfWorkersAsync
    requestProgressUpdateAsync = C . requestProgressUpdateAsync
-- }}}

-- Driver {{{
driver :: Driver IO configuration result
driver = Driver
    (genericDriver runVisitorMaybeStartingFrom)
    (genericDriver runVisitorIOMaybeStartingFrom)
    (genericDriver . runVisitorTMaybeStartingFrom)
  where
    genericDriver run configuration_parser (infomod :: ∀ α. InfoMod α) initializeGlobalState getMaybeStartingProgress notifyTerminated constructVisitor constructManager = do
        configuration ← execParser (info configuration_parser infomod)
        initializeGlobalState configuration
        maybe_starting_progress ← getMaybeStartingProgress configuration
        run  maybe_starting_progress
            (constructVisitor configuration)
            (changeNumberOfWorkersToMatchCPUs >> constructManager configuration)
         >>= notifyTerminated configuration
-- }}}

-- Exposed Functions {{{

changeNumberOfWorkersToMatchCPUs :: ThreadsControllerMonad result () -- {{{
changeNumberOfWorkersToMatchCPUs =
    liftIO getNumCapabilities >>= \n → changeNumberOfWorkersAsync (const (return n)) (void . return)
-- }}}

runVisitor :: -- {{{
    Monoid result ⇒
    Visitor result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitor = runVisitorMaybeStartingFrom Nothing
-- }}}

runVisitorMaybeStartingFrom :: -- {{{
    Monoid result ⇒
    Maybe (VisitorProgress result) →
    Visitor result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorMaybeStartingFrom maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip forkVisitorWorkerThread
-- }}}

runVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    Visitor result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorStartingFrom = runVisitorMaybeStartingFrom . Just
-- }}}

runVisitorIO :: -- {{{
    Monoid result ⇒
    VisitorIO result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorIO = runVisitorIOMaybeStartingFrom Nothing
-- }}}

runVisitorIOMaybeStartingFrom :: -- {{{
    Monoid result ⇒
    Maybe (VisitorProgress result) →
    VisitorIO result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorIOMaybeStartingFrom maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip forkVisitorIOWorkerThread
-- }}}

runVisitorIOStartingFrom :: -- {{{
    Monoid result ⇒
    VisitorProgress result →
    VisitorIO result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorIOStartingFrom = runVisitorIOMaybeStartingFrom . Just
-- }}}

runVisitorT :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorT m result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorT = flip runVisitorTMaybeStartingFrom Nothing
-- }}}

runVisitorTMaybeStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    Maybe (VisitorProgress result) →
    VisitorT m result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorTMaybeStartingFrom runMonad maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip (forkVisitorTWorkerThread runMonad)
-- }}}

runVisitorTStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorProgress result →
    VisitorT m result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorTStartingFrom runInIO = runVisitorTMaybeStartingFrom runInIO . Just
-- }}}

-- }}}

-- Logging Functions {{{
logger_name = "Threads"

debugM, infoM :: MonadIO m ⇒ String → m ()
debugM = liftIO . Logger.debugM logger_name
infoM = liftIO . Logger.infoM logger_name
-- }}}

-- Internal Functions {{{

fromJustOrBust message = fromMaybe (error message)

genericRunVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    Maybe (VisitorProgress result) →
    (
        (VisitorWorkerTerminationReason result → IO ()) →
        VisitorWorkload →
        IO (VisitorWorkerEnvironment result)
    ) →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
genericRunVisitorStartingFrom maybe_starting_progress spawnWorker (C controller) =
    runWorkgroup
        mempty
        (\WorkgroupReceivers{..} →
            let createWorker _ = return ()
                destroyWorker worker_id False = return ()
                destroyWorker worker_id True = do -- {{{
                    get >>=
                        liftIO
                        .
                        sendAbortRequest
                        .
                        workerPendingRequests
                        .
                        fromJustOrBust ("destroyWorker: active record for worker " ++ show worker_id ++ " not found")
                        .
                        IntMap.lookup worker_id
                    modify (IntMap.delete worker_id)
                -- }}}
                killAllWorkers _ = -- {{{
                    get >>=
                        liftIO
                        .
                        mapM_ (killThread . workerThreadId)
                        .
                        IntMap.elems
                -- }}}
                sendProgressUpdateRequest worker_id = -- {{{
                    get >>=
                        liftIO
                        .
                        maybe (return ()) (
                            flip Worker.sendProgressUpdateRequest (receiveProgressUpdateFromWorker worker_id)
                            .
                            workerPendingRequests
                        )
                        .
                        IntMap.lookup worker_id
                -- }}}
                sendWorkloadStealRequest worker_id = -- {{{
                    get >>=
                        liftIO
                        .
                        maybe (return ()) (
                            flip Worker.sendWorkloadStealRequest (receiveStolenWorkloadFromWorker worker_id)
                            .
                            workerPendingRequests
                        )
                        .
                        IntMap.lookup worker_id
                -- }}}
                sendWorkloadToWorker worker_id workload = -- {{{
                    (liftIO $ spawnWorker (\termination_reason →
                        case termination_reason of
                            VisitorWorkerFinished final_progress →
                                receiveFinishedFromWorker worker_id final_progress
                            VisitorWorkerFailed message →
                                receiveFailureFromWorker worker_id message
                            VisitorWorkerAborted →
                                receiveQuitFromWorker worker_id
                    ) workload)
                    >>=
                    modify
                    .
                    IntMap.insert worker_id
                -- }}}
            in WorkgroupCallbacks{..}
        )
        maybe_starting_progress
        controller
-- }}}

-- }}}
