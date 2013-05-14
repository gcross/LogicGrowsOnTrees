-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.BackEnd.Threads
    ( ThreadsControllerMonad
    , abort
    , changeNumberOfWorkers
    , changeNumberOfWorkersAsync
    , changeNumberOfWorkersToMatchCPUs
    , driver
    , fork
    , getCurrentProgress
    , getCurrentProgressAsync
    , getNumberOfWorkers
    , getNumberOfWorkersAsync
    , requestProgressUpdate
    , requestProgressUpdateAsync
    , runVisitor
    , runVisitorStartingFrom
    , runVisitorIO
    , runVisitorIOStartingFrom
    , runVisitorT
    , runVisitorTStartingFrom
    ) where

-- Imports {{{
import Control.Applicative (Applicative,liftA2)
import Control.Concurrent (forkIO,getNumCapabilities,killThread)
import Control.Monad (void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Parallel.Main (Driver(Driver),RunOutcome,mainParser)
import Control.Visitor.Parallel.Common.Supervisor.RequestQueue
import Control.Visitor.Parallel.Common.VisitorMode
import Control.Visitor.Parallel.Common.Worker as Worker hiding (runVisitor,runVisitorIO,runVisitorT)
import Control.Visitor.Parallel.Common.Workgroup
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Types {{{
newtype ThreadsControllerMonad r iv ip fv fp α =
    C { unwrapC :: WorkgroupControllerMonad (IntMap (WorkerEnvironment ip)) r iv ip fv fp α
      } deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,WorkgroupRequestQueueMonad)
-- }}}

-- Instances {{{
instance RequestQueueMonad (ThreadsControllerMonad r iv ip fv fp) where
    type RequestQueueMonadIntermediateProgress (ThreadsControllerMonad r iv ip fv fp) = ip
    abort = C abort
    fork = C . fork . unwrapC
    getCurrentProgressAsync = C . getCurrentProgressAsync
    getNumberOfWorkersAsync = C . getNumberOfWorkersAsync
    requestProgressUpdateAsync = C . requestProgressUpdateAsync
-- }}}

-- Driver {{{
driver :: Driver IO shared_configuration supervisor_configuration visitor r iv ip fv fp
driver = Driver $
    \visitor_mode
     forkVisitorWorkerThread
     shared_configuration_term
     supervisor_configuration_term
     term_info
     initializeGlobalState
     constructVisitor
     getMaybeStartingProgress
     notifyTerminated
     constructManager →
 do (shared_configuration,supervisor_configuration) ←
        mainParser (liftA2 (,) shared_configuration_term supervisor_configuration_term) term_info
    initializeGlobalState shared_configuration
    starting_progress ← getMaybeStartingProgress shared_configuration supervisor_configuration
    genericRunVisitorStartingFrom
         visitor_mode
         starting_progress
        (flip forkVisitorWorkerThread . constructVisitor $ shared_configuration)
        (changeNumberOfWorkersToMatchCPUs >> constructManager shared_configuration supervisor_configuration)
     >>= notifyTerminated shared_configuration supervisor_configuration
-- }}}

-- Exposed Functions {{{

changeNumberOfWorkersToMatchCPUs :: ThreadsControllerMonad r iv ip fv fp () -- {{{
changeNumberOfWorkersToMatchCPUs =
    liftIO getNumCapabilities >>= \n → changeNumberOfWorkersAsync (const (return n)) (void . return)
-- }}}

runVisitor :: -- {{{
    Monoid result ⇒
    Visitor result →
    ThreadsControllerMonad result result (Progress result) result (Progress result) () →
    IO (RunOutcome (Progress result) result)
runVisitor = runVisitorStartingFrom mempty
-- }}}

runVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
    Visitor result →
    ThreadsControllerMonad result result (Progress result) result (Progress result) () →
    IO (RunOutcome (Progress result) result)
runVisitorStartingFrom starting_progress =
    genericRunVisitorStartingFrom AllMode starting_progress
    .
    flip forkVisitorWorkerThread
-- }}}

runVisitorIO :: -- {{{
    Monoid result ⇒
    VisitorIO result →
    ThreadsControllerMonad result result (Progress result) result (Progress result) () →
    IO (RunOutcome (Progress result) result)
runVisitorIO = runVisitorIOStartingFrom mempty
-- }}}

runVisitorIOStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
    VisitorIO result →
    ThreadsControllerMonad result result (Progress result) result (Progress result) () →
    IO (RunOutcome (Progress result) result)
runVisitorIOStartingFrom starting_progress =
    genericRunVisitorStartingFrom AllMode starting_progress
    .
    flip forkVisitorIOWorkerThread
-- }}}

runVisitorT :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorT m result →
    ThreadsControllerMonad result result (Progress result) result (Progress result) () →
    IO (RunOutcome (Progress result) result)
runVisitorT = flip runVisitorTStartingFrom mempty
-- }}}

runVisitorTStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    Progress result →
    VisitorT m result →
    ThreadsControllerMonad result result (Progress result) result (Progress result) () →
    IO (RunOutcome (Progress result) result)
runVisitorTStartingFrom runMonad starting_progress =
    genericRunVisitorStartingFrom AllMode starting_progress
    .
    flip (forkVisitorTWorkerThread runMonad)
-- }}}

-- }}}

-- Internal Functions {{{

fromJustOrBust message = fromMaybe (error message)

genericRunVisitorStartingFrom :: -- {{{
    VisitorMode r iv ip fv fp →
    ip →
    (
        (WorkerTerminationReason fp → IO ()) →
        Workload →
        IO (WorkerEnvironment ip)
    ) →
    ThreadsControllerMonad r iv ip fv fp () →
    IO (RunOutcome ip fv)
genericRunVisitorStartingFrom visitor_mode starting_progress spawnWorker (C controller) =
    runWorkgroup
        visitor_mode
        mempty
        (\MessageForSupervisorReceivers{..} →
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
                    (debugM $ "Sending " ++ show workload ++ " to worker " ++ show worker_id)
                    >>
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
                    >>
                    (debugM $ "Thread for worker " ++ show worker_id ++ "started.")
                -- }}}
            in WorkgroupCallbacks{..}
        )
        starting_progress
        controller
-- }}}

-- }}}
