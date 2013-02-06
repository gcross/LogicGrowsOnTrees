-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Threads where

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
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Main (Driver(Driver),TerminationReason)
import Control.Visitor.Parallel.Workgroup
import Control.Visitor.Supervisor.RequestQueue
import Control.Visitor.Worker as Worker
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Types {{{
newtype ThreadsControllerMonad result α = C { unwrapC :: WorkgroupControllerMonad (IntMap (WorkerEnvironment result)) result α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,WorkgroupRequestQueueMonad)
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
driver :: Driver IO configuration visitor result
driver = Driver $ \forkWorkerThread configuration_parser infomod initializeGlobalState getMaybeStartingProgress notifyTerminated constructVisitor constructManager → do
    configuration ← execParser (info configuration_parser infomod)
    initializeGlobalState configuration
    maybe_starting_progress ← getMaybeStartingProgress configuration
    genericRunVisitorStartingFrom
         maybe_starting_progress
        (flip forkWorkerThread . constructVisitor $ configuration)
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
    Maybe (Progress result) →
    Visitor result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorMaybeStartingFrom maybe_starting_progress =
    genericRunVisitorStartingFrom maybe_starting_progress
    .
    flip forkWorkerThread
-- }}}

runVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
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
    Maybe (Progress result) →
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
    Progress result →
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
    Maybe (Progress result) →
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
    Progress result →
    VisitorT m result →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
runVisitorTStartingFrom runInIO = runVisitorTMaybeStartingFrom runInIO . Just
-- }}}

-- }}}

-- Internal Functions {{{

fromJustOrBust message = fromMaybe (error message)

genericRunVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    Maybe (Progress result) →
    (
        (WorkerTerminationReason result → IO ()) →
        Workload →
        IO (WorkerEnvironment result)
    ) →
    ThreadsControllerMonad result () →
    IO (TerminationReason result)
genericRunVisitorStartingFrom maybe_starting_progress spawnWorker (C controller) =
    runWorkgroup
        mempty
        (\WorkgroupReceivers{..} →
            let createWorker _ = return ()
                destroyWorker worker_id False = liftIO $ receiveQuitFromWorker worker_id
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
                sendRequestToWorker request receiver worker_id = -- {{{
                    get >>=
                        liftIO
                        .
                        maybe (return ()) (
                            flip request (receiver worker_id)
                            .
                            workerPendingRequests
                        )
                        .
                        IntMap.lookup worker_id
                -- }}}
                sendProgressUpdateRequestTo = sendRequestToWorker Worker.sendProgressUpdateRequest receiveProgressUpdateFromWorker
                sendWorkloadStealRequestTo = sendRequestToWorker Worker.sendWorkloadStealRequest receiveStolenWorkloadFromWorker
                sendWorkloadTo worker_id workload = -- {{{
                    (liftIO $ spawnWorker (\termination_reason →
                        case termination_reason of
                            WorkerFinished final_progress →
                                receiveFinishedFromWorker worker_id final_progress
                            WorkerFailed message →
                                receiveFailureFromWorker worker_id message
                            WorkerAborted →
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
