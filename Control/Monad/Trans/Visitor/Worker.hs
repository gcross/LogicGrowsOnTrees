-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Worker where

-- Imports {{{
import Prelude hiding (catch)

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
import Control.Exception (AsyncException(ThreadKilled),SomeException,catch,evaluate,fromException)
import Control.Monad (liftM)
import Control.Monad.IO.Class

import Data.Bool.Higher ((??))
import Data.Composition
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode((⊕))
import Data.Maybe (isJust)
import Data.Sequence ((|>),(><),Seq,viewl,ViewL(..))
import qualified Data.Sequence as Seq

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Types {{{

data VisitorWorkerEnvironment α = VisitorWorkerEnvironment -- {{{
    {   workerInitialPath :: VisitorPath
    ,   workerThreadId :: ThreadId
    ,   workerPendingRequests :: VisitorWorkerRequestQueue α
    ,   workerTerminationFlag :: IVar ()
    }
-- }}}

data VisitorWorkerRequest α = -- {{{
    AbortRequested
  | ProgressUpdateRequested (VisitorWorkerProgressUpdate α → IO ())
  | WorkloadStealRequested (Maybe (VisitorWorkerStolenWorkload α) → IO ())
-- }}}

type VisitorWorkerRequestQueue α = IORef [VisitorWorkerRequest α]

data VisitorWorkerProgressUpdate α = VisitorWorkerProgressUpdate -- {{{
    {   visitorWorkerProgressUpdate :: VisitorProgress α
    ,   visitorWorkerRemainingWorkload :: VisitorWorkload
    } deriving (Eq,Show)
-- }}}

data VisitorWorkerStolenWorkload α = VisitorWorkerStolenWorkload -- {{{
    {   visitorWorkerStolenWorkerProgressUpdate :: VisitorWorkerProgressUpdate α
    ,   visitorWorkerStolenWorkload :: VisitorWorkload
    } deriving (Eq,Show)
-- }}}

data VisitorWorkerTerminationReason α = -- {{{
    VisitorWorkerFinished (VisitorProgress α)
  | VisitorWorkerFailed SomeException
  | VisitorWorkerAborted
  deriving (Show)
-- }}}

-- }}}

-- Functions {{{

computeProgressUpdate :: -- {{{
    α →
    VisitorPath →
    VisitorCheckpointCursor →
    VisitorTContext m α →
    VisitorCheckpoint →
    VisitorWorkerProgressUpdate α
computeProgressUpdate result initial_path cursor context checkpoint =
    VisitorWorkerProgressUpdate
        (VisitorProgress
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
        (VisitorWorkload (initial_path >< pathFromCursor cursor)
         .
         checkpointFromContext context
         $
         checkpoint
        )
-- }}}

forkVisitorIOWorkerThread :: -- {{{
    Monoid α ⇒
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorIO α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkVisitorIOWorkerThread = forkVisitorTWorkerThread id
-- }}}

forkVisitorTWorkerThread :: -- {{{
    (Functor m, MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorT m α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkVisitorTWorkerThread =
    genericForkVisitorTWorkerThread
        sendVisitorTDownPath
        stepVisitorTThroughCheckpoint
-- }}}

forkVisitorWorkerThread :: -- {{{
    Monoid α ⇒
    (VisitorWorkerTerminationReason α → IO ()) →
    Visitor α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkVisitorWorkerThread =
    genericForkVisitorTWorkerThread
        (return .* sendVisitorDownPath)
        (return .** stepVisitorThroughCheckpoint)
        id
-- }}}

genericForkVisitorTWorkerThread :: -- {{{
    (MonadIO n, Monoid α) ⇒
    (
        VisitorPath → VisitorT m α → n (VisitorT m α)
    ) →
    (
        VisitorTContext m α →
        VisitorCheckpoint →
        VisitorT m α →
        n (Maybe α,Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α))
    ) →
    (∀ β. n β → IO β) →
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorT m α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
genericForkVisitorTWorkerThread
    walk
    step
    run
    finishedCallback
    visitor
    workload
  = do (start,environment) ← genericPreforkVisitorTWorkerThread walk step run finishedCallback visitor workload
       start
       return environment
-- }}}

genericPreforkVisitorTWorkerThread :: -- {{{
    (MonadIO n, Monoid α) ⇒
    (
        VisitorPath → VisitorT m α → n (VisitorT m α)
    ) →
    (
        VisitorTContext m α →
        VisitorCheckpoint →
        VisitorT m α →
        n (Maybe α,Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α))
    ) →
    (∀ β. n β → IO β) →
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorT m α →
    VisitorWorkload →
    IO (IO (), VisitorWorkerEnvironment α)
genericPreforkVisitorTWorkerThread
    walk
    step
    run
    finishedCallback
    visitor
    (VisitorWorkload initial_path initial_checkpoint)
  = do
    pending_requests_ref ← newIORef []
    let initial_label = labelFromPath initial_path
        loop1
            cursor
            context
            result
            checkpoint
            visitor
          = liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                [] → loop3
                        cursor
                        context
                        result
                        checkpoint
                        visitor
                _ → (liftM reverse . liftIO $ atomicModifyIORef pending_requests_ref (const [] &&& id))
                    >>= loop2
                            cursor
                            context
                            result
                            checkpoint
                            visitor
        loop2
            cursor
            context
            result
            checkpoint
            visitor
            requests
          = case requests of
            -- Respond to request {{{
                [] → do
                    liftIO yield
                    loop3
                        cursor
                        context
                        result
                        checkpoint
                        visitor
                AbortRequested:_ → return VisitorWorkerAborted
                ProgressUpdateRequested submitProgress:rest_requests → do
                    liftIO . submitProgress $ computeProgressUpdate result initial_path cursor context checkpoint
                    loop2
                        cursor
                        context
                        mempty
                        checkpoint
                        visitor
                        rest_requests
                WorkloadStealRequested submitMaybeWorkload:rest_requests →
                    case tryStealWorkload initial_path cursor context of
                        Nothing → do
                            liftIO $ submitMaybeWorkload Nothing
                            loop2
                                cursor
                                context
                                result
                                checkpoint
                                visitor
                                rest_requests
                        Just (new_cursor,new_context,workload) → do
                            liftIO $ do
                                submitMaybeWorkload (Just (
                                    VisitorWorkerStolenWorkload
                                        (computeProgressUpdate result initial_path new_cursor new_context checkpoint)
                                        workload
                                 ))
                            loop2
                                new_cursor
                                new_context
                                mempty
                                checkpoint
                                visitor
                                rest_requests
            -- }}}
        loop3
            cursor
            context
            result
            checkpoint
            visitor
          = do
            -- Step visitor {{{
            (maybe_solution,maybe_next) ← step context checkpoint visitor
            new_result ← liftIO $ do
                case maybe_solution of
                    Nothing → return result
                    Just solution → evaluate $ solution `seq` (result ⊕ solution)
            case maybe_next of
                Nothing →
                    return
                    .
                    VisitorWorkerFinished
                    .
                    flip VisitorProgress new_result
                    .
                    checkpointFromInitialPath initial_path
                    .
                    checkpointFromCursor cursor
                    $
                    Explored
                Just (new_context,new_checkpoint,new_visitor) →
                    loop1
                        cursor
                        new_context
                        new_result
                        new_checkpoint
                        new_visitor
            -- }}}
    start_flag_ivar ← IVar.new
    finished_flag ← IVar.new
    thread_id ← forkIO $ do
        termination_reason ←
                (do IVar.blocking . IVar.read $ start_flag_ivar
                    run $
                        walk initial_path visitor
                        >>=
                        loop1
                            Seq.empty
                            Seq.empty
                            mempty
                            initial_checkpoint
                )
                `catch`
                (\e → case fromException e of
                    Just ThreadKilled → return VisitorWorkerAborted
                    _ → return $ VisitorWorkerFailed e
                )
        IVar.write finished_flag ()
        finishedCallback termination_reason
    return
        (IVar.write start_flag_ivar ()
        ,VisitorWorkerEnvironment
            initial_path
            thread_id
            pending_requests_ref
            finished_flag
        )
-- }}}

preforkVisitorIOWorkerThread :: -- {{{
    Monoid α ⇒
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorIO α →
    VisitorWorkload →
    IO (IO (), VisitorWorkerEnvironment α)
preforkVisitorIOWorkerThread = preforkVisitorTWorkerThread id
-- }}}

preforkVisitorTWorkerThread :: -- {{{
    (Functor m, MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorT m α →
    VisitorWorkload →
    IO (IO (),VisitorWorkerEnvironment α)
preforkVisitorTWorkerThread =
    genericPreforkVisitorTWorkerThread
        sendVisitorTDownPath
        stepVisitorTThroughCheckpoint
-- }}}

preforkVisitorWorkerThread :: -- {{{
    Monoid α ⇒
    (VisitorWorkerTerminationReason α → IO ()) →
    Visitor α →
    VisitorWorkload →
    IO (IO (), VisitorWorkerEnvironment α)
preforkVisitorWorkerThread =
    genericPreforkVisitorTWorkerThread
        (return .* sendVisitorDownPath)
        (return .** stepVisitorThroughCheckpoint)
        id
-- }}}

sendAbortRequest :: VisitorWorkerRequestQueue α → IO () -- {{{
sendAbortRequest = flip sendRequest AbortRequested
-- }}}

sendProgressUpdateRequest :: -- {{{
    VisitorWorkerRequestQueue α →
    (VisitorWorkerProgressUpdate α → IO ()) →
    IO ()
sendProgressUpdateRequest queue = sendRequest queue . ProgressUpdateRequested
-- }}}

sendRequest :: VisitorWorkerRequestQueue α → VisitorWorkerRequest α → IO () -- {{{
sendRequest queue request = atomicModifyIORef queue ((request:) &&& const ())
-- }}}

sendWorkloadStealRequest :: -- {{{
    VisitorWorkerRequestQueue α →
    (Maybe (VisitorWorkerStolenWorkload α) → IO ()) →
    IO ()
sendWorkloadStealRequest queue = sendRequest queue . WorkloadStealRequested
-- }}}

tryStealWorkload :: -- {{{
    VisitorPath →
    VisitorCheckpointCursor →
    VisitorTContext m α →
    Maybe (VisitorCheckpointCursor,VisitorTContext m α,VisitorWorkload)
tryStealWorkload initial_path = go
  where
    go _      (viewl → EmptyL) = Nothing
    go cursor (viewl → step :< rest_context) = case step of
        BranchContextStep active_branch →
            go (cursor |> ChoiceCheckpointD active_branch Explored) rest_context
        CacheContextStep cache →
            go (cursor |> CacheCheckpointD cache) rest_context
        LeftChoiceContextStep other_checkpoint _ →
            Just
                (cursor |> ChoiceCheckpointD LeftBranch Unexplored
                ,rest_context
                ,VisitorWorkload
                    ((initial_path >< pathFromCursor cursor) |> ChoiceStep RightBranch)
                    other_checkpoint
                )
-- }}}

-- }}}
