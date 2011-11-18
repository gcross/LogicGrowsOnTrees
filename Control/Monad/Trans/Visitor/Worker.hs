-- @+leo-ver=5-thin
-- @+node:gcross.20110923164140.1252: * @file Control/Monad/Trans/Visitor/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923164140.1253: ** << Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Worker where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923164140.1254: ** << Import needed modules >>
import Prelude hiding (catch)

import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
import Control.Exception (AsyncException,SomeException,catch,catchJust,evaluate,fromException)
import Control.Monad.IO.Class

import Data.Bool.Higher ((??))
import Data.Composition
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Maybe (isJust)
import Data.Sequence ((|>),(><),Seq,viewl,ViewL(..))
import qualified Data.Sequence as Seq

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
import Control.Monad.Trans.Visitor.Workload
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923164140.1255: ** Types
-- @+node:gcross.20110923164140.1257: *3* VisitorWorkerEnvironment
data VisitorWorkerEnvironment α = VisitorWorkerEnvironment
    {   workerInitialPath :: VisitorPath
    ,   workerThreadId :: ThreadId
    ,   workerPendingRequests :: IORef (VisitorWorkerRequestQueue α)
    }
-- @+node:gcross.20111117140347.1414: *3* VisitorWorkerFinalUpdate
data VisitorWorkerFinalUpdate α = VisitorWorkerFinalUpdate
    {   visitorWorkerFinalNewSolutions :: [VisitorSolution α]
    ,   visitorWorkerFinalGlobalCheckpoint :: VisitorCheckpoint
    } deriving (Eq,Show)
-- @+node:gcross.20111004110500.1246: *3* VisitorWorkerRequest
data VisitorWorkerRequest α =
    StatusUpdateRequested (Maybe (VisitorWorkerStatusUpdate α) → IO ())
  | WorkloadStealRequested (Maybe (VisitorWorkerStatusUpdate α,VisitorWorkload) → IO ())
-- @+node:gcross.20111026172030.1278: *3* VisitorWorkerRequestQueue
type VisitorWorkerRequestQueue α = Maybe (Seq (VisitorWorkerRequest α))
-- @+node:gcross.20111020182554.1275: *3* VisitorWorkerStatusUpdate
data VisitorWorkerStatusUpdate α = VisitorWorkerStatusUpdate
    {   visitorWorkerNewSolutions :: [VisitorSolution α]
    ,   visitorWorkerRemainingWorkload :: VisitorWorkload
    ,   visitorWorkerGlobalCheckpoint :: VisitorCheckpoint
    } deriving (Eq,Show)
-- @+node:gcross.20111020182554.1276: *3* VisitorWorkerTerminationReason
data VisitorWorkerTerminationReason α =
    VisitorWorkerFinished (VisitorWorkerFinalUpdate α)
  | VisitorWorkerFailed SomeException
  | VisitorWorkerAborted
  deriving (Show)
-- @+node:gcross.20110923164140.1259: ** Functions
-- @+node:gcross.20111117140347.1410: *3* computeStatusUpdate
computeStatusUpdate ::
    DList (VisitorSolution α) →
    VisitorPath →
    VisitorCheckpointCursor →
    VisitorTContext m α →
    VisitorCheckpoint →
    VisitorWorkerStatusUpdate α
computeStatusUpdate solutions initial_path cursor context checkpoint =
    VisitorWorkerStatusUpdate
        (DList.toList solutions)
        (VisitorWorkload (initial_path >< pathFromCursor cursor)
         .
         checkpointFromContext context
         $
         checkpoint
        )
        (checkpointFromInitialPath initial_path
         .
         checkpointFromCursor cursor
         .
         checkpointFromContext context
         $
         checkpoint
        )
-- @+node:gcross.20111117140347.1400: *3* forkVisitorIOWorkerThread
forkVisitorIOWorkerThread ::
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorIO α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkVisitorIOWorkerThread = forkVisitorTWorkerThread id
-- @+node:gcross.20111026220221.1454: *3* forkVisitorTWorkerThread
forkVisitorTWorkerThread ::
    (Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorT m α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkVisitorTWorkerThread =
    genericForkVisitorTWorkerThread
        walkVisitorTDownPath
        stepVisitorTThroughCheckpoint
-- @+node:gcross.20111026220221.1456: *3* forkVisitorWorkerThread
forkVisitorWorkerThread ::
    (VisitorWorkerTerminationReason α → IO ()) →
    Visitor α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkVisitorWorkerThread =
    genericForkVisitorTWorkerThread
        (return .* walkVisitorDownPath)
        (return .** stepVisitorThroughCheckpoint)
        id
-- @+node:gcross.20110923164140.1286: *3* genericForkVisitorTWorkerThread
genericForkVisitorTWorkerThread ::
    MonadIO n ⇒
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
-- @+node:gcross.20111117140347.1392: *3* genericPreforkVisitorTWorkerThread
genericPreforkVisitorTWorkerThread ::
    MonadIO n ⇒
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
            solutions
            checkpoint
            visitor
          = liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                Nothing → return VisitorWorkerAborted
                Just (viewl → _ :< _) → do
                    -- @+<< Respond to request >>
                    -- @+node:gcross.20111117140347.1393: *4* << Respond to request >>
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
                                solutions
                                checkpoint
                                visitor
                        Just (StatusUpdateRequested submitMaybeStatusUpdate) → do
                            liftIO $ do
                                submitMaybeStatusUpdate . Just $ computeStatusUpdate solutions initial_path cursor context checkpoint
                                yield
                            loop2
                                cursor
                                context
                                DList.empty
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
                                        solutions
                                        checkpoint
                                        visitor
                                Just (new_cursor,new_context,workload) → do
                                    liftIO $ do
                                        submitMaybeWorkload (Just (computeStatusUpdate solutions initial_path new_cursor new_context checkpoint,workload))
                                        yield
                                    loop2
                                        new_cursor
                                        new_context
                                        DList.empty
                                        checkpoint
                                        visitor
                    -- @-<< Respond to request >>
                _ → loop2
                        cursor
                        context
                        solutions
                        checkpoint
                        visitor
        loop2
            cursor
            context
            solutions
            checkpoint
            visitor
          = do
            -- @+<< Step visitor >>
            -- @+node:gcross.20111117140347.1394: *4* << Step visitor >>
            (maybe_solution,maybe_next) ← step context checkpoint visitor
            new_solutions ← liftIO . evaluate . ($ solutions) $
                case maybe_solution of
                    Nothing → id
                    Just solution →
                        let label =
                                applyContextToLabel context
                                .
                                applyCheckpointCursorToLabel cursor
                                $
                                initial_label
                        in solution `seq` label `seq` (flip DList.snoc $ VisitorSolution label solution)
            case maybe_next of
                Nothing →
                    return
                    .
                    VisitorWorkerFinished
                    .
                    VisitorWorkerFinalUpdate (DList.toList new_solutions)
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
                        new_solutions
                        new_checkpoint
                        new_visitor
            -- @-<< Step visitor >>
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
                            DList.empty
                            initial_checkpoint
                )
                (return . VisitorWorkerFailed)
        atomicModifyIORef pending_requests_ref (Nothing,)
            >>=
            maybe
                (return ())
                (Fold.mapM_ $ \request → case request of
                    StatusUpdateRequested submitMaybeStatusUpdate → submitMaybeStatusUpdate Nothing
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
-- @+node:gcross.20111028181213.1331: *3* preforkVisitorIOWorkerThread
preforkVisitorIOWorkerThread ::
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorIO α →
    VisitorWorkload →
    IO (IO (), VisitorWorkerEnvironment α)
preforkVisitorIOWorkerThread = preforkVisitorTWorkerThread id
-- @+node:gcross.20111117140347.1398: *3* preforkVisitorTWorkerThread
preforkVisitorTWorkerThread ::
    (Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    (VisitorWorkerTerminationReason α → IO ()) →
    VisitorT m α →
    VisitorWorkload →
    IO (IO (),VisitorWorkerEnvironment α)
preforkVisitorTWorkerThread =
    genericPreforkVisitorTWorkerThread
        walkVisitorTDownPath
        stepVisitorTThroughCheckpoint
-- @+node:gcross.20111117140347.1396: *3* preforkVisitorWorkerThread
preforkVisitorWorkerThread ::
    (VisitorWorkerTerminationReason α → IO ()) →
    Visitor α →
    VisitorWorkload →
    IO (IO (), VisitorWorkerEnvironment α)
preforkVisitorWorkerThread =
    genericPreforkVisitorTWorkerThread
        (return .* walkVisitorDownPath)
        (return .** stepVisitorThroughCheckpoint)
        id
-- @+node:gcross.20110923164140.1260: *3* tryStealWorkload
tryStealWorkload ::
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
-- @-others
-- @-leo
