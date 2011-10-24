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
import Control.Exception (SomeException,catch)

import Data.Bool.Higher ((??))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Maybe (isJust)
import Data.Sequence ((|>),(><),Seq,viewl,ViewL(..))
import qualified Data.Sequence as Seq

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923164140.1255: ** Types
-- @+node:gcross.20110923164140.1257: *3* VisitorTWorkerEnvironment
data VisitorTWorkerEnvironment m α = VisitorTWorkerEnvironment
    {   workerInitialPath :: VisitorPath
    ,   workerThreadId :: ThreadId
    ,   workerPendingRequest :: IORef (Maybe (VisitorWorkerRequest α))
    ,   workerTerminationStatus :: IVar (VisitorWorkerTerminationReason α)
    }
type VisitorWorkerEnvironment α = VisitorTWorkerEnvironment Identity α
-- @+node:gcross.20111004110500.1246: *3* VisitorWorkerRequest
data VisitorWorkerRequest α =
    AbortWorker
  | StatusUpdateRequested (VisitorWorkerStatusUpdate α → IO ())
  | WorkloadStealRequested (VisitorWorkload → IO ()) (IO ())
-- @+node:gcross.20111020182554.1275: *3* VisitorWorkerStatusUpdate
data VisitorWorkerStatusUpdate α = VisitorWorkerStatusUpdate
    {   visitorWorkerNewSolutions :: [VisitorSolution α]
    ,   visitorWorkerRemainingWorkload :: VisitorWorkload
    }
-- @+node:gcross.20111020182554.1276: *3* VisitorWorkerTerminationReason
data VisitorWorkerTerminationReason α =
    VisitorWorkerFinished [VisitorSolution α]
  | VisitorWorkerFailed SomeException
  | VisitorWorkerAborted
-- @+node:gcross.20110923164140.1261: *3* VisitorWorkload
data VisitorWorkload = VisitorWorkload
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: VisitorCheckpoint
    }
-- @+node:gcross.20110923164140.1259: ** Functions
-- @+node:gcross.20110923164140.1286: *3* forkWorkerThread
forkWorkerThread ::
    Visitor α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkWorkerThread visitor (VisitorWorkload initial_path initial_checkpoint) = do
    pending_request_ref ← newIORef $ Nothing
    termination_status_ivar ← IVar.new
    let initial_label = labelFromPath initial_path
        loop
            cursor
            context
            solutions
            checkpoint
            visitor
          = fmap isJust (readIORef pending_request_ref) >>=
            (
                -- @+<< Process request >>
                -- @+node:gcross.20111020182554.1282: *4* << Process request >>
                atomicModifyIORef pending_request_ref (Nothing,)
                >>=
                \request → case request of
                    Nothing → do
                        yield
                        loop
                            cursor
                            context
                            solutions
                            checkpoint
                            visitor
                    Just AbortWorker → IVar.write termination_status_ivar VisitorWorkerAborted
                    Just (StatusUpdateRequested submitStatusUpdate) → do
                        submitStatusUpdate $
                            VisitorWorkerStatusUpdate
                                (DList.toList solutions)
                                (VisitorWorkload initial_path
                                 .
                                 checkpointFromCursor cursor
                                 .
                                 checkpointFromContext context
                                 $
                                 checkpoint
                                )
                        yield
                        loop
                            cursor
                            context
                            DList.empty
                            checkpoint
                            visitor
                    Just (WorkloadStealRequested submitWorkload notifyFailure) →
                        case tryStealWorkload initial_path context of
                            Nothing → do
                                notifyFailure
                                yield
                                loop
                                    cursor
                                    context
                                    solutions
                                    checkpoint
                                    visitor
                            Just (append_to_cursor,new_context,workload) → do
                                submitWorkload workload
                                yield
                                loop
                                    (cursor >< append_to_cursor)
                                    new_context
                                    solutions
                                    checkpoint
                                    visitor
                -- @-<< Process request >>
            ) ?? (
                -- @+<< Step visitor >>
                -- @+node:gcross.20111020182554.1283: *4* << Step visitor >>
                let (maybe_solution,updateContext) = stepVisitorThroughCheckpoint checkpoint visitor
                    new_solutions =
                        case maybe_solution of
                            Nothing → new_solutions
                            Just solution → new_solutions `DList.snoc`
                                VisitorSolution
                                    (
                                        applyContextToLabel context
                                        .
                                        applyCheckpointCursorToLabel cursor
                                        $
                                        initial_label
                                    )
                                    solution
                in case updateContext context of
                    Nothing → do
                        IVar.write termination_status_ivar (VisitorWorkerFinished (DList.toList new_solutions))
                    Just (new_context,new_checkpoint,new_visitor) →
                        loop
                            cursor
                            new_context
                            new_solutions
                            new_checkpoint
                            new_visitor
                -- @-<< Step visitor >>
            )
    thread_id ← forkIO $
        (loop
            Seq.empty
            Seq.empty
            DList.empty
            initial_checkpoint
         .
         walkVisitorDownPath initial_path
         $
         visitor
        )
        `catch`
        (IVar.write termination_status_ivar . VisitorWorkerFailed)
    return $
        VisitorTWorkerEnvironment
            initial_path
            thread_id
            pending_request_ref
            termination_status_ivar
-- @+node:gcross.20110923164140.1260: *3* tryStealWorkload
tryStealWorkload ::
    VisitorPath →
    VisitorTContext m α →
    Maybe (VisitorCheckpointCursor,VisitorTContext m α,VisitorWorkload)
tryStealWorkload initial_path = go Seq.empty
  where
    go _      (viewl → EmptyL) = Nothing
    go cursor (viewl → step :< rest_context) = case step of
        BranchContextStep active_branch →
            go (cursor |> ChoiceCheckpointD active_branch Explored) rest_context
        CacheContextStep cache →
            go (cursor |> CacheCheckpointD cache) rest_context
        LeftChoiceContextStep other_checkpoint _ →
            let new_cursor = (cursor |> ChoiceCheckpointD RightBranchActive other_checkpoint)
            in Just
                (new_cursor
                ,rest_context
                ,VisitorWorkload
                    (initial_path >< pathFromCursor new_cursor)
                    other_checkpoint
                )
-- @-others
-- @-leo
