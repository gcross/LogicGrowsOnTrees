-- Language extensions {{{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Parallel.BackEnd.Threads
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
    , visitTree
    , visitTreeStartingFrom
    , visitTreeIO
    , visitTreeIOStartingFrom
    , visitTreeT
    , visitTreeTStartingFrom
    , visitTreeUntilFirst
    , visitTreeUntilFirstStartingFrom
    , visitTreeIOUntilFirst
    , visitTreeIOUntilFirstStartingFrom
    , visitTreeTUntilFirst
    , visitTreeTUntilFirstStartingFrom
    , visitTreeUntilFoundUsingPull
    , visitTreeUntilFoundUsingPullStartingFrom
    , visitTreeIOUntilFoundUsingPull
    , visitTreeIOUntilFoundUsingPullStartingFrom
    , visitTreeTUntilFoundUsingPull
    , visitTreeTUntilFoundUsingPullStartingFrom
    , visitTreeUntilFoundUsingPush
    , visitTreeUntilFoundUsingPushStartingFrom
    , visitTreeIOUntilFoundUsingPush
    , visitTreeIOUntilFoundUsingPushStartingFrom
    , visitTreeTUntilFoundUsingPush
    , visitTreeTUntilFoundUsingPushStartingFrom
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
import Data.Void (absurd)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import Visitor (TreeGenerator,TreeGeneratorIO,TreeGeneratorT)
import Visitor.Checkpoint
import Visitor.Parallel.Main (Driver(Driver),RunOutcome,RunOutcomeFor,mainParser)
import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.VisitorMode
import Visitor.Parallel.Common.Worker as Worker
    hiding
    (visitTree
    ,visitTreeIO
    ,visitTreeT
    ,visitTreeUntilFirst
    ,visitTreeIOUntilFirst
    ,visitTreeTUntilFirst
    )
import Visitor.Parallel.Common.Workgroup hiding (C,unwrapC)
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

visitTree :: -- {{{
    Monoid result ⇒
    TreeGenerator result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
visitTree = visitTreeStartingFrom mempty
-- }}}

visitTreeStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
    TreeGenerator result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
visitTreeStartingFrom = launchVisitorStartingFrom AllMode PureVisitor
-- }}}

visitTreeIO :: -- {{{
    Monoid result ⇒
    TreeGeneratorIO result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
visitTreeIO = visitTreeIOStartingFrom mempty
-- }}}

visitTreeIOStartingFrom :: -- {{{
    Monoid result ⇒
    Progress result →
    TreeGeneratorIO result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
visitTreeIOStartingFrom = launchVisitorStartingFrom AllMode IOVisitor
-- }}}

visitTreeT :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    TreeGeneratorT m result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
visitTreeT = flip visitTreeTStartingFrom mempty
-- }}}

visitTreeTStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    Progress result →
    TreeGeneratorT m result →
    ThreadsControllerMonad (AllMode result) () →
    IO (RunOutcome (Progress result) result)
visitTreeTStartingFrom = launchVisitorStartingFrom AllMode  . ImpureVisitor
-- }}}

visitTreeUntilFirst :: -- {{{
    TreeGenerator result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
visitTreeUntilFirst = visitTreeUntilFirstStartingFrom mempty
-- }}}

visitTreeUntilFirstStartingFrom :: -- {{{
    Checkpoint →
    TreeGenerator result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
visitTreeUntilFirstStartingFrom = launchVisitorStartingFrom FirstMode PureVisitor
-- }}}

visitTreeIOUntilFirst :: -- {{{
    TreeGeneratorIO result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
visitTreeIOUntilFirst = visitTreeIOUntilFirstStartingFrom mempty
-- }}}

visitTreeIOUntilFirstStartingFrom :: -- {{{
    Checkpoint →
    TreeGeneratorIO result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
visitTreeIOUntilFirstStartingFrom = launchVisitorStartingFrom FirstMode IOVisitor
-- }}}

visitTreeTUntilFirst :: -- {{{
    MonadIO m ⇒
    (∀ α. m α → IO α) →
    TreeGeneratorT m result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
visitTreeTUntilFirst = flip visitTreeTUntilFirstStartingFrom mempty
-- }}}

visitTreeTUntilFirstStartingFrom :: -- {{{
    MonadIO m ⇒
    (∀ α. m α → IO α) →
    Checkpoint →
    TreeGeneratorT m result →
    ThreadsControllerMonad (FirstMode result) () →
    IO (RunOutcome Checkpoint (Maybe (Progress result)))
visitTreeTUntilFirstStartingFrom = launchVisitorStartingFrom FirstMode . ImpureVisitor
-- }}}

visitTreeUntilFoundUsingPull :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGenerator result →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
visitTreeUntilFoundUsingPull = flip visitTreeUntilFoundUsingPullStartingFrom mempty
-- }}}

visitTreeUntilFoundUsingPullStartingFrom :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result →
    TreeGenerator result →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
visitTreeUntilFoundUsingPullStartingFrom f = launchVisitorStartingFrom (FoundModeUsingPull f) PureVisitor
-- }}}

visitTreeIOUntilFoundUsingPull :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGeneratorIO result →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
visitTreeIOUntilFoundUsingPull = flip visitTreeIOUntilFoundUsingPullStartingFrom mempty
-- }}}

visitTreeIOUntilFoundUsingPullStartingFrom :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result →
    TreeGeneratorIO result →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
visitTreeIOUntilFoundUsingPullStartingFrom f = launchVisitorStartingFrom (FoundModeUsingPull f) IOVisitor
-- }}}

visitTreeTUntilFoundUsingPull :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) →
    TreeGeneratorT m result →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
visitTreeTUntilFoundUsingPull f run = visitTreeTUntilFoundUsingPullStartingFrom f run mempty
-- }}}

visitTreeTUntilFoundUsingPullStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) →
    Progress result →
    TreeGeneratorT m result →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result))))
visitTreeTUntilFoundUsingPullStartingFrom f = launchVisitorStartingFrom (FoundModeUsingPull f) . ImpureVisitor
-- }}}

visitTreeUntilFoundUsingPush :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGenerator result →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress final_result)))
visitTreeUntilFoundUsingPush = flip visitTreeUntilFoundUsingPushStartingFrom mempty
-- }}}

visitTreeUntilFoundUsingPushStartingFrom :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result →
    TreeGenerator result →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress final_result)))
visitTreeUntilFoundUsingPushStartingFrom f = launchVisitorStartingFrom (FoundModeUsingPush f) PureVisitor
-- }}}

visitTreeIOUntilFoundUsingPush :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGeneratorIO result →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress final_result)))
visitTreeIOUntilFoundUsingPush = flip visitTreeIOUntilFoundUsingPushStartingFrom mempty
-- }}}

visitTreeIOUntilFoundUsingPushStartingFrom :: -- {{{
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result →
    TreeGeneratorIO result →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress final_result)))
visitTreeIOUntilFoundUsingPushStartingFrom f = launchVisitorStartingFrom (FoundModeUsingPush f) IOVisitor
-- }}}

visitTreeTUntilFoundUsingPush :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) →
    TreeGeneratorT m result →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress final_result)))
visitTreeTUntilFoundUsingPush f run = visitTreeTUntilFoundUsingPushStartingFrom f run mempty
-- }}}

visitTreeTUntilFoundUsingPushStartingFrom :: -- {{{
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) →
    Progress result →
    TreeGeneratorT m result →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () →
    IO (RunOutcome (Progress result) (Either result (Progress final_result)))
visitTreeTUntilFoundUsingPushStartingFrom f = launchVisitorStartingFrom (FoundModeUsingPush f) . ImpureVisitor
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
    TreeGeneratorT m (ResultFor visitor_mode) →
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
                            (case visitor_mode of
                                AllMode → absurd
                                FirstMode → absurd
                                FoundModeUsingPull _ → absurd
                                FoundModeUsingPush _ → receiveProgressUpdateFromWorker worker_id
                            )
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
