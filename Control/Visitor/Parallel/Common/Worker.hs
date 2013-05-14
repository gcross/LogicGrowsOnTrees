-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Parallel.Common.Worker
    ( ProgressUpdate(..)
    , StolenWorkload(..)
    , WorkerRequestQueue
    , WorkerEnvironment(..)
    , WorkerRunTerminationReason
    , WorkerSearchTerminationReason
    , WorkerTerminationReason(..)
    , forkVisitorWorkerThread
    , forkVisitorIOWorkerThread
    , forkVisitorTWorkerThread
    , runVisitor
    , runVisitorIO
    , runVisitorT
    , sendAbortRequest
    , sendProgressUpdateRequest
    , sendWorkloadStealRequest
    ) where

-- Imports {{{
import Prelude hiding (catch)

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),catch,evaluate,fromException)
import Control.Monad (liftM)
import Control.Monad.IO.Class

import Data.Bool.Higher ((??))
import Data.Composition
import Data.Derive.Serialize
import Data.DeriveTH
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Monoid (Monoid(..))
import Data.Maybe (isJust)
import Data.Sequence ((|>),(><),Seq,viewl,ViewL(..))
import Data.Serialize
import qualified Data.Sequence as Seq

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor hiding (runVisitor,runVisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Parallel.Common.ResultType
import Control.Visitor.Path
import Control.Visitor.Workload
-- }}}

-- Types {{{

data ProgressUpdate α = ProgressUpdate -- {{{
    {   progressUpdateProgress :: RunProgress α
    ,   progressUpdateRemainingWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''ProgressUpdate )
-- }}}

data StolenWorkload α = StolenWorkload -- {{{
    {   stolenWorkloadProgressUpdate :: ProgressUpdate α
    ,   stolenWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''StolenWorkload )
-- }}}

data WorkerRequest α = -- {{{
    AbortRequested
  | ProgressUpdateRequested (ProgressUpdate α → IO ())
  | WorkloadStealRequested (Maybe (StolenWorkload α) → IO ())
-- }}}

type WorkerRequestQueue α = IORef [WorkerRequest α]

data WorkerEnvironment α = WorkerEnvironment -- {{{
    {   workerInitialPath :: Path
    ,   workerThreadId :: ThreadId
    ,   workerPendingRequests :: WorkerRequestQueue α
    ,   workerTerminationFlag :: IVar ()
    }
-- }}}

data WorkerTerminationReason result_kind result = -- {{{
    WorkerFinished (FinalProgressTypeOf result_kind result)
  | WorkerFailed String
  | WorkerAborted
-- }}}

instance Show (FinalProgressTypeOf result_kind result) ⇒ Show (WorkerTerminationReason result_kind result) where
    show (WorkerFinished final_progress) = "WorkerFinished (" ++ show final_progress ++ ")"
    show (WorkerFailed reason) = "WorkerFailed " ++ show reason
    show (WorkerAborted) = "WorkerAborted"

type WorkerRunTerminationReason = WorkerTerminationReason RunResult
type WorkerSearchTerminationReason = WorkerTerminationReason RunResult

-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Functions {{{

computeProgressUpdate :: -- {{{
    β →
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    ProgressUpdate β
computeProgressUpdate result initial_path cursor context checkpoint =
    ProgressUpdate
        (RunProgress
            (checkpointFromInitialPath initial_path
             .
             checkpointFromCursor cursor
             .
             checkpointFromContext context
             $
             checkpoint
            )
            result
        )
        (Workload (initial_path >< pathFromCursor cursor)
         .
         checkpointFromContext context
         $
         checkpoint
        )
-- }}}

forkVisitorWorkerThread :: -- {{{
    Monoid α ⇒
    (WorkerRunTerminationReason α → IO ()) →
    Visitor α →
    Workload →
    IO (WorkerEnvironment α)
forkVisitorWorkerThread =
    genericForkVisitorTWorkerThread
        RunResult
        (return .* sendVisitorDownPath)
        (return . stepVisitorThroughCheckpoint)
        id
-- }}}

forkVisitorIOWorkerThread :: -- {{{
    Monoid α ⇒
    (WorkerRunTerminationReason α → IO ()) →
    VisitorIO α →
    Workload →
    IO (WorkerEnvironment α)
forkVisitorIOWorkerThread = forkVisitorTWorkerThread id
-- }}}

forkVisitorTWorkerThread :: -- {{{
    (MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    (WorkerRunTerminationReason α → IO ()) →
    VisitorT m α →
    Workload →
    IO (WorkerEnvironment α)
forkVisitorTWorkerThread =
    genericForkVisitorTWorkerThread
        RunResult
        sendVisitorTDownPath
        stepVisitorTThroughCheckpoint
-- }}}

genericForkVisitorTWorkerThread :: -- {{{
    MonadIO n ⇒
    ResultType result_kind result →
    (Path → VisitorT m result → n (VisitorT m result)) →
    (VisitorTState m result → n (Maybe result,Maybe (VisitorTState m result))) →
    (∀ ξ. n ξ → IO ξ) →
    (WorkerTerminationReason result_kind result → IO ()) →
    VisitorT m result →
    Workload →
    IO (WorkerEnvironment (IntermediateTypeOf result_kind result))
genericForkVisitorTWorkerThread
    result_type
    walk
    step
    run
    finishedCallback
    visitor
    (Workload initial_path initial_checkpoint)
  = do
    pending_requests_ref ← newIORef []
    let loop1 result cursor visitor_state = -- Check for requests {{{
            liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                [] → loop3 result cursor visitor_state
                _ → debugM "Worker thread's request queue is non-empty."
                    >> (liftM reverse . liftIO $ atomicModifyIORef pending_requests_ref (const [] &&& id))
                    >>= loop2 result cursor visitor_state
        -- }}}
        loop2 result cursor visitor_state@(VisitorTState context checkpoint visitor) requests = -- Process requests {{{
            case requests of
                [] → liftIO yield >> loop3 result cursor visitor_state
                AbortRequested:_ → do
                    debugM "Worker theread received abort request."
                    return WorkerAborted
                ProgressUpdateRequested submitProgress:rest_requests → do
                    debugM "Worker thread received progress update request."
                    liftIO . submitProgress $ computeProgressUpdate result initial_path cursor context checkpoint
                    loop2 (initialIntermediateOf result_type) cursor visitor_state rest_requests
                WorkloadStealRequested submitMaybeWorkload:rest_requests → do
                    debugM "Worker thread received workload steal."
                    case tryStealWorkload initial_path cursor context of
                        Nothing → do
                            liftIO $ submitMaybeWorkload Nothing
                            loop2 result cursor visitor_state rest_requests
                        Just (new_cursor,new_context,workload) → do
                            liftIO . submitMaybeWorkload . Just $
                                StolenWorkload
                                    (computeProgressUpdate result initial_path new_cursor new_context checkpoint)
                                    workload
                            loop2 (initialIntermediateOf result_type) new_cursor (VisitorTState new_context checkpoint visitor) rest_requests
        -- }}}
        loop3 result cursor visitor_state -- Step visitor {{{
          = do
            (maybe_solution,maybe_new_visitor_state) ← step visitor_state
            case maybe_new_visitor_state of
                Nothing →
                    let final_checkpoint =
                            checkpointFromInitialPath initial_path
                            .
                            checkpointFromCursor cursor
                            $
                            Explored
                    in return . WorkerFinished $
                        case result_type of
                            RunResult →
                                RunProgress
                                    final_checkpoint
                                    (maybe result (mappend result) maybe_solution)
                            SearchResult → SearchIncomplete final_checkpoint
                Just new_visitor_state →
                    case maybe_solution of
                        Nothing → loop1 result cursor new_visitor_state
                        Just solution →
                            case result_type of
                                RunResult → do
                                    new_result ← liftIO . evaluate $ solution `seq` (result `mappend` solution)
                                    loop1 new_result cursor new_visitor_state
                                SearchResult →
                                    return
                                    .
                                    WorkerFinished
                                    .
                                    SearchComplete
                                    $
                                    solution
        -- }}}
    start_flag_ivar ← IVar.new
    finished_flag ← IVar.new
    thread_id ← forkIO $ do
        termination_reason ←
            debugM "Worker thread has been forked." >>
            (run $
                walk initial_path visitor
                >>=
                loop1 (initialIntermediateOf result_type) Seq.empty
                .
                initialVisitorState initial_checkpoint
            )
            `catch`
            (\e → case fromException e of
                Just ThreadKilled → return WorkerAborted
                Just UserInterrupt → return WorkerAborted
                _ → return $ WorkerFailed (show e)
            )
        debugM $ "Worker thread has terminated with reason " ++
            case termination_reason of
                WorkerFinished _ → "finished."
                WorkerFailed message → "failed: " ++ show message
                WorkerAborted → "aborted."
        IVar.write finished_flag ()
        finishedCallback termination_reason
    return $
        WorkerEnvironment
            initial_path
            thread_id
            pending_requests_ref
            finished_flag
{-# INLINE genericForkVisitorTWorkerThread #-}
-- }}}

genericRunVisitor :: -- {{{
    Monoid α ⇒
    ((WorkerRunTerminationReason α → IO ()) → Workload → IO (WorkerEnvironment α)) →
    IO (WorkerRunTerminationReason α)
genericRunVisitor forkWorkerThread = do
    result_mvar ← newEmptyMVar
    _ ← forkWorkerThread
            (putMVar result_mvar)
            entire_workload
    takeMVar result_mvar
-- }}}

runVisitor :: Monoid α ⇒ Visitor α → IO (WorkerRunTerminationReason α) -- {{{
runVisitor = genericRunVisitor . flip forkVisitorWorkerThread
-- }}}

runVisitorIO :: Monoid α ⇒ VisitorIO α → IO (WorkerRunTerminationReason α) -- {{{
runVisitorIO = genericRunVisitor . flip forkVisitorIOWorkerThread
-- }}}

runVisitorT :: (Monoid α, MonadIO m) ⇒ (∀ β. m β → IO β) → VisitorT m α → IO (WorkerRunTerminationReason α) -- {{{
runVisitorT runInIO = genericRunVisitor . flip (forkVisitorTWorkerThread runInIO)
-- }}}

sendAbortRequest :: WorkerRequestQueue α → IO () -- {{{
sendAbortRequest = flip sendRequest AbortRequested
-- }}}

sendProgressUpdateRequest :: -- {{{
    WorkerRequestQueue α →
    (ProgressUpdate α → IO ()) →
    IO ()
sendProgressUpdateRequest queue = sendRequest queue . ProgressUpdateRequested
-- }}}

sendRequest :: WorkerRequestQueue α → WorkerRequest α → IO () -- {{{
sendRequest queue request = atomicModifyIORef queue ((request:) &&& const ())
-- }}}

sendWorkloadStealRequest :: -- {{{
    WorkerRequestQueue α →
    (Maybe (StolenWorkload α) → IO ()) →
    IO ()
sendWorkloadStealRequest queue = sendRequest queue . WorkloadStealRequested
-- }}}

tryStealWorkload :: -- {{{
    Path →
    CheckpointCursor →
    Context m α →
    Maybe (CheckpointCursor,Context m α,Workload)
tryStealWorkload initial_path = go
  where
    go _      (viewl → EmptyL) = Nothing
    go cursor (viewl → step :< rest_context) = case step of
        CacheContextStep cache →
            go (cursor |> CacheCheckpointD cache) rest_context
        LeftBranchContextStep other_checkpoint _ →
            Just
                (cursor |> ChoiceCheckpointD LeftBranch Unexplored
                ,rest_context
                ,Workload
                    ((initial_path >< pathFromCursor cursor) |> ChoiceStep RightBranch)
                    other_checkpoint
                )
        RightBranchContextStep →
            go (cursor |> ChoiceCheckpointD RightBranch Explored) rest_context
-- }}}

-- }}}
