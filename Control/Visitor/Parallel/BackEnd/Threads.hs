-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
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
    , launchVisitorStartingFrom
    , requestProgressUpdate
    , requestProgressUpdateAsync
    , runVisitor
    , runVisitorStartingFrom
    , runVisitorIO
    , runVisitorIOStartingFrom
    , runVisitorT
    , runVisitorTStartingFrom
    , runVisitorUntilFirst
    , runVisitorUntilFirstStartingFrom
    , runVisitorIOUntilFirst
    , runVisitorIOUntilFirstStartingFrom
    , runVisitorTUntilFirst
    , runVisitorTUntilFirstStartingFrom
    , runVisitorUntilFound
    , runVisitorUntilFoundStartingFrom
    , runVisitorIOUntilFound
    , runVisitorIOUntilFoundStartingFrom
    , runVisitorTUntilFound
    , runVisitorTUntilFoundStartingFrom
    ) where

-- Imports {{{
import Control.Applicative (Applicative,liftA2)
import Control.Concurrent (getNumCapabilities,killThread)
import Control.Monad (void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import Control.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Parallel.Main (Driver(Driver),RunOutcome,RunOutcomeFor,mainParser)
import Control.Visitor.Parallel.Common.Supervisor.RequestQueue
import Control.Visitor.Parallel.Common.VisitorMode
import Control.Visitor.Parallel.Common.Worker as Worker
    hiding
    (runVisitor
    ,runVisitorIO
    ,runVisitorT
    ,runVisitorUntilFirst
    ,runVisitorIOUntilFirst
    ,runVisitorTUntilFirst
    )
import Control.Visitor.Parallel.Common.Workgroup hiding (C,unwrapC)
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG]
-- }}}

-- Types {{{
newtype ThreadsControllerMonad visitor_mode α =
    C { unwrapC :: WorkgroupControllerMonad (IntMap (WorkerEnvironment (ProgressFor visitor_mode))) visitor_mode α
      } deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad,WorkgroupRequestQueueMonad)
-- }}}

-- Instances {{{
instance HasVisitorMode (ThreadsControllerMonad visitor_mode) where -- {{{
    type VisitorModeFor (ThreadsControllerMonad visitor_mode) = visitor_mode
-- }}}
-- }}}

-- Driver {{{
driver :: Driver IO shared_configuration supervisor_configuration m n visitor_mode
driver = Driver $
    \visitor_mode
     visitor_kind
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
    launchVisitorStartingFrom
         visitor_mode
         visitor_kind
         starting_progress
        (constructVisitor shared_configuration)
        (changeNumberOfWorkersToMatchCPUs >> constructManager shared_configuration supervisor_configuration)
     >>= notifyTerminated shared_configuration supervisor_configuration
-- }}}

-- Exposed Functions {{{

changeNumberOfWorkersToMatchCPUs :: ThreadsControllerMonad visitor_mode () -- {{{
changeNumberOfWorkersToMatchCPUs =
    liftIO getNumCapabilities >>= \n → changeNumberOfWorkersAsync (const (return n)) (void . return)
-- }}}

runVisitor :: -- {{{
    Monoid result ⇒
    Visitor result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
runVisitor = runVisitorStartingFrom mempty
-- }}}

runVisitorStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
    Visitor result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
runVisitorStartingFrom = launchVisitorStartingFrom AllMode PureVisitor
-- }}}

runVisitorIO :: -- {{{
    Monoid result ⇒
    VisitorIO result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
runVisitorIO = runVisitorIOStartingFrom mempty
-- }}}

runVisitorIOStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
    VisitorIO result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
runVisitorIOStartingFrom = launchVisitorStartingFrom AllMode IOVisitor
-- }}}

runVisitorT :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorT m result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
runVisitorT = flip runVisitorTStartingFrom mempty
-- }}}

runVisitorTStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    Progress result →
    VisitorT m result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
runVisitorTStartingFrom = launchVisitorStartingFrom AllMode  . ImpureVisitor
-- }}}

runVisitorUntilFirst :: -- {{{
    Visitor result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
runVisitorUntilFirst = runVisitorUntilFirstStartingFrom mempty
-- }}}

runVisitorUntilFirstStartingFrom :: -- {{{
    Checkpoint →
    Visitor result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
runVisitorUntilFirstStartingFrom = launchVisitorStartingFrom FirstMode PureVisitor
-- }}}

runVisitorIOUntilFirst :: -- {{{
    VisitorIO result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
runVisitorIOUntilFirst = runVisitorIOUntilFirstStartingFrom mempty
-- }}}

runVisitorIOUntilFirstStartingFrom :: -- {{{
    Checkpoint →
    VisitorIO result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
runVisitorIOUntilFirstStartingFrom = launchVisitorStartingFrom FirstMode IOVisitor
-- }}}

runVisitorTUntilFirst :: -- {{{
    MonadIO m ⇒
    (∀ α. m α → IO α) →
    VisitorT m result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
runVisitorTUntilFirst = flip runVisitorTUntilFirstStartingFrom mempty
-- }}}

runVisitorTUntilFirstStartingFrom :: -- {{{
    MonadIO m ⇒
    (∀ α. m α → IO α) →
    Checkpoint →
    VisitorT m result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
runVisitorTUntilFirstStartingFrom = launchVisitorStartingFrom FirstMode . ImpureVisitor
-- }}}

runVisitorUntilFound :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Visitor result →
    ThreadsControllerMonad (FoundMode result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
runVisitorUntilFound = flip runVisitorUntilFoundStartingFrom mempty
-- }}}

runVisitorUntilFoundStartingFrom :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result →
    Visitor result →
    ThreadsControllerMonad (FoundMode result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
runVisitorUntilFoundStartingFrom f = launchVisitorStartingFrom (FoundMode f) PureVisitor
-- }}}

runVisitorIOUntilFound :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    VisitorIO result →
    ThreadsControllerMonad (FoundMode result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
runVisitorIOUntilFound = flip runVisitorIOUntilFoundStartingFrom mempty
-- }}}

runVisitorIOUntilFoundStartingFrom :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result →
    VisitorIO result →
    ThreadsControllerMonad (FoundMode result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
runVisitorIOUntilFoundStartingFrom f = launchVisitorStartingFrom (FoundMode f) IOVisitor
-- }}}

runVisitorTUntilFound :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) →
    VisitorT m result →
    ThreadsControllerMonad (FoundMode result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
runVisitorTUntilFound f run = runVisitorTUntilFoundStartingFrom f run mempty
-- }}}

runVisitorTUntilFoundStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) →
    Progress result →
    VisitorT m result →
    ThreadsControllerMonad (FoundMode result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
runVisitorTUntilFoundStartingFrom f = launchVisitorStartingFrom (FoundMode f) . ImpureVisitor
-- }}}

-- }}}

-- Internal Functions {{{

fromJustOrBust :: String → Maybe α → α -- {{{
fromJustOrBust message = fromMaybe (error message)
-- }}}

launchVisitorStartingFrom :: -- {{{
    VisitorMode visitor_mode →
    VisitorKind m n →
    (ProgressFor visitor_mode) →
    VisitorT m (ResultFor visitor_mode) →
    ThreadsControllerMonad visitor_mode () →
    IO (RunOutcomeFor visitor_mode)
launchVisitorStartingFrom visitor_mode visitor_kind starting_progress visitor (C controller) =
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
                    (liftIO $
                        forkWorkerThread
                            visitor_mode
                            visitor_kind
                            (\termination_reason →
                                case termination_reason of
                                    WorkerFinished final_progress →
                                        receiveFinishedFromWorker worker_id final_progress
                                    WorkerFailed message →
                                        receiveFailureFromWorker worker_id message
                                    WorkerAborted →
                                        receiveQuitFromWorker worker_id
                            )
                            visitor
                            workload
                    )
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
