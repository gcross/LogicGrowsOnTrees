-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Worker where

-- Imports {{{
import Prelude hiding (catch)

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
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

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Path
import Control.Visitor.Workload
-- }}}

-- Types {{{

data ProgressUpdate α = ProgressUpdate -- {{{
    {   progressUpdateProgress :: Progress α
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

data WorkerTerminationReason α = -- {{{
    WorkerFinished (Progress α)
  | WorkerFailed String
  | WorkerAborted
  deriving (Show)
-- }}}

-- }}}

-- Functions {{{

computeProgressUpdate :: -- {{{
    α →
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    ProgressUpdate α
computeProgressUpdate result initial_path cursor context checkpoint =
    ProgressUpdate
        (Progress
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
    (WorkerTerminationReason α → IO ()) →
    Visitor α →
    Workload →
    IO (WorkerEnvironment α)
forkVisitorWorkerThread =
    genericForkVisitorTWorkerThread
        (return .* sendVisitorDownPath)
        (return . stepVisitorThroughCheckpoint)
        id
-- }}}

forkVisitorIOWorkerThread :: -- {{{
    Monoid α ⇒
    (WorkerTerminationReason α → IO ()) →
    VisitorIO α →
    Workload →
    IO (WorkerEnvironment α)
forkVisitorIOWorkerThread = forkVisitorTWorkerThread id
-- }}}

forkVisitorTWorkerThread :: -- {{{
    (MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    (WorkerTerminationReason α → IO ()) →
    VisitorT m α →
    Workload →
    IO (WorkerEnvironment α)
forkVisitorTWorkerThread =
    genericForkVisitorTWorkerThread
        sendVisitorTDownPath
        stepVisitorTThroughCheckpoint
-- }}}

genericForkVisitorTWorkerThread :: -- {{{
    (MonadIO n, Monoid α) ⇒
    (Path → VisitorT m α → n (VisitorT m α)) →
    (VisitorTState m α → n (Maybe α,Maybe (VisitorTState m α))) →
    (∀ β. n β → IO β) →
    (WorkerTerminationReason α → IO ()) →
    VisitorT m α →
    Workload →
    IO (WorkerEnvironment α)
genericForkVisitorTWorkerThread
    walk
    step
    run
    finishedCallback
    visitor
    (Workload initial_path initial_checkpoint)
  = do
    pending_requests_ref ← newIORef []
    let loop1 result cursor visitor_state =
            liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                [] → loop3 result cursor visitor_state
                _ → (liftM reverse . liftIO $ atomicModifyIORef pending_requests_ref (const [] &&& id))
                    >>= loop2 result cursor visitor_state
        loop2 result cursor visitor_state@(VisitorTState context checkpoint visitor) requests =
          case requests of
            -- Respond to request {{{
                [] → liftIO yield >> loop3 result cursor visitor_state
                AbortRequested:_ → return WorkerAborted
                ProgressUpdateRequested submitProgress:rest_requests → do
                    liftIO . submitProgress $ computeProgressUpdate result initial_path cursor context checkpoint
                    loop2 mempty cursor visitor_state rest_requests
                WorkloadStealRequested submitMaybeWorkload:rest_requests →
                    case tryStealWorkload initial_path cursor context of
                        Nothing → do
                            liftIO $ submitMaybeWorkload Nothing
                            loop2 result cursor visitor_state rest_requests
                        Just (new_cursor,new_context,workload) → do
                            liftIO . submitMaybeWorkload . Just $
                                StolenWorkload
                                    (computeProgressUpdate result initial_path new_cursor new_context checkpoint)
                                    workload
                            loop2 mempty new_cursor (VisitorTState new_context checkpoint visitor) rest_requests
            -- }}}
        loop3 result cursor visitor_state
          = do
            -- Step visitor {{{
            (maybe_solution,maybe_new_visitor_state) ← step visitor_state
            new_result ← liftIO $ do
                case maybe_solution of
                    Nothing → return result
                    Just solution → evaluate $ solution `seq` (result `mappend` solution)
            case maybe_new_visitor_state of
                Nothing →
                    return
                    .
                    WorkerFinished
                    .
                    flip Progress new_result
                    .
                    checkpointFromInitialPath initial_path
                    .
                    checkpointFromCursor cursor
                    $
                    Explored
                Just new_visitor_state → loop1 new_result cursor new_visitor_state
            -- }}}
    start_flag_ivar ← IVar.new
    finished_flag ← IVar.new
    thread_id ← forkIO $ do
        termination_reason ←
                (run $
                    walk initial_path visitor
                    >>=
                    loop1 mempty Seq.empty . initialVisitorState initial_checkpoint
                )
                `catch`
                (\e → case fromException e of
                    Just ThreadKilled → return WorkerAborted
                    Just UserInterrupt → return WorkerAborted
                    _ → return $ WorkerFailed (show e)
                )
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
