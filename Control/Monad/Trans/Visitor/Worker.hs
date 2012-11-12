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

import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
import Control.Exception (AsyncException,SomeException,catch,catchJust,evaluate,fromException)
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
    ,   workerPendingRequests :: IORef (VisitorWorkerRequestQueue α)
    }
-- }}}

data VisitorWorkerRequest α = -- {{{
    ProgressUpdateRequested (Maybe (VisitorWorkerProgressUpdate α) → IO ())
  | WorkloadStealRequested (Maybe (VisitorWorkerStolenWorkload α) → IO ())
-- }}}

type VisitorWorkerRequestQueue α = Maybe (Seq (VisitorWorkerRequest α))

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

attemptAddToWorkerRequestQueue :: -- {{{
    IORef (VisitorWorkerRequestQueue α) →
    VisitorWorkerRequest α →
    IO Bool

attemptAddToWorkerRequestQueue request_queue element =
    atomicModifyIORef
        request_queue
        (maybe
            (Nothing,False)
            ((,True) . Just . (|> element))
        )
--}}}

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
    pending_requests_ref ← newIORef $ (Just Seq.empty)
    let initial_label = labelFromPath initial_path
        loop1
            cursor
            context
            result
            checkpoint
            visitor
          = liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                Nothing → return VisitorWorkerAborted
                Just (viewl → _ :< _) → do
                    -- Respond to request {{{
                    request ← liftIO $
                        atomicModifyIORef pending_requests_ref (
                            \maybe_requests → case fmap viewl maybe_requests of
                                Nothing → (Nothing,Nothing)
                                Just EmptyL → (Just Seq.empty,Nothing)
                                Just (request :< rest_requests) → (Just rest_requests,Just request)
                        )
                    case request of
                        Nothing → do
                            liftIO yield
                            loop2
                                cursor
                                context
                                result
                                checkpoint
                                visitor
                        Just (ProgressUpdateRequested submitMaybeProgress) → do
                            liftIO $ do
                                submitMaybeProgress . Just $ computeProgressUpdate result initial_path cursor context checkpoint
                                yield
                            loop2
                                cursor
                                context
                                mempty
                                checkpoint
                                visitor
                        Just (WorkloadStealRequested submitMaybeWorkload) →
                            case tryStealWorkload initial_path cursor context of
                                Nothing → do
                                    liftIO $ do
                                        submitMaybeWorkload Nothing
                                        yield
                                    loop2
                                        cursor
                                        context
                                        result
                                        checkpoint
                                        visitor
                                Just (new_cursor,new_context,workload) → do
                                    liftIO $ do
                                        submitMaybeWorkload (Just (
                                            VisitorWorkerStolenWorkload
                                                (computeProgressUpdate result initial_path new_cursor new_context checkpoint)
                                                workload
                                         ))
                                        yield
                                    loop2
                                        new_cursor
                                        new_context
                                        mempty
                                        checkpoint
                                        visitor
                    -- }}}
                _ → loop2
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
    thread_id ← forkIO $ do
        termination_reason ←
            catchJust
                (\e → case fromException e of {Just (_ :: AsyncException) → Nothing; _ → Just e})
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
                (return . VisitorWorkerFailed)
        atomicModifyIORef pending_requests_ref (Nothing,)
            >>=
            maybe
                (return ())
                (Fold.mapM_ $ \request → case request of
                    ProgressUpdateRequested submitMaybeProgress → submitMaybeProgress Nothing
                    WorkloadStealRequested submitMaybeWorkload → submitMaybeWorkload Nothing
                )
        finishedCallback termination_reason
    return
        (IVar.write start_flag_ivar ()
        ,VisitorWorkerEnvironment
            initial_path
            thread_id
            pending_requests_ref
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
