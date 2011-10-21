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

import Control.Applicative ((<$>),(<*>),liftA2,liftA3)
import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
import Control.Exception (AsyncException(ThreadKilled),SomeException,evaluate)
import Control.Monad (liftM2,liftM3,mzero)
import Control.Monad.CatchIO (finally,MonadCatchIO(catch),throw)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops

import Data.Bool.Higher ((??))
import Data.Composition
import Data.Foldable as Fold
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Sequence ((|>),(><),Seq,viewl,ViewL(..))
import qualified Data.Sequence as Seq

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923164140.1255: ** Types
-- @+node:gcross.20110923164140.1262: *3* VisitorPartialCheckpoint
type VisitorPartialCheckpoint = VisitorCheckpoint → VisitorCheckpoint
-- @+node:gcross.20110923164140.1257: *3* VisitorTWorkerEnvironment
data VisitorTWorkerEnvironment m α = VisitorTWorkerEnvironment
    {   workerInitialPath :: VisitorPath
    ,   workerThreadId :: ThreadId
    ,   workerCurrentProgress :: IORef (VisitorTWorkerProgress m α)
    ,   workerTerminationStatus :: IVar (Maybe SomeException)
    }
type VisitorWorkerEnvironment α = VisitorTWorkerEnvironment Identity α
-- @+node:gcross.20111020151748.1289: *3* VisitorTWorkerProgress
data VisitorTWorkerProgress m α = VisitorTWorkerProgress
    {   workerFrozenCheckpointCursor :: VisitorCheckpointCursor
    ,   workerContext :: VisitorTContext m α
    ,   workerRemainingCheckpoint :: VisitorCheckpoint
    ,   workerNewSolutions :: Seq (VisitorSolution α)
    }
type VisitorWorkerProgress = VisitorTWorkerProgress Identity
-- @+node:gcross.20111004110500.1246: *3* VisitorWorkerRequest
data VisitorWorkerRequest m α =
    AbortWorker
  | StatusUpdateRequested (VisitorWorkerStatusUpdate α → m ())
  | WorkloadStealRequested (VisitorWorkload → m ()) (m ())
-- @+node:gcross.20111020182554.1275: *3* VisitorWorkerStatusUpdate
data VisitorWorkerStatusUpdate α = VisitorWorkerStatusUpdate
    {   visitorWorkerNewSolutions :: [VisitorSolution α]
    ,   visitorWorkerRemainingWorkload :: VisitorWorkload
    }
-- @+node:gcross.20111020182554.1276: *3* VisitorWorkerTerminationReason
data VisitorWorkerTerminationReason =
    VisitorWorkerFinished
  | VisitorWorkerFailed SomeException
  | VisitorWorkerAborted
-- @+node:gcross.20110923164140.1261: *3* VisitorWorkload
data VisitorWorkload = VisitorWorkload
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: VisitorCheckpoint
    }
-- @+node:gcross.20110923164140.1259: ** Functions
-- @+node:gcross.20110923164140.1287: *3* computeRemainingWorkload(UsingEnvironment)
computeRemainingWorkload ::
    VisitorPath →
    VisitorCheckpointCursor →
    VisitorTContext m α →
    VisitorCheckpoint →
    VisitorWorkload
computeRemainingWorkload initial_path cursor context =
    VisitorWorkload initial_path
    .
    checkpointFromCursor cursor
    .
    checkpointFromContext context

computeRemainingWorkloadUsingProgress ::
    VisitorPath →
    VisitorTWorkerProgress m α →
    VisitorWorkload
computeRemainingWorkloadUsingProgress initial_path =
    computeRemainingWorkload initial_path
        <$> workerFrozenCheckpointCursor
        <*> workerContext
        <*> workerRemainingCheckpoint

computeRemainingWorkloadUsingEnvironment ::
    VisitorTWorkerEnvironment m α →
    IO VisitorWorkload
computeRemainingWorkloadUsingEnvironment =
    fmap
        <$> (computeRemainingWorkloadUsingProgress . workerInitialPath)
        <*> (readIORef . workerCurrentProgress)
-- @+node:gcross.20111020182554.1274: *3* constructWorkerStatusUpdate(AndUpdateEnvironment)
constructWorkerStatusUpdate ::
    VisitorPath →
    VisitorTWorkerProgress m α →
    (VisitorTWorkerProgress m α, VisitorWorkerStatusUpdate α)
constructWorkerStatusUpdate initial_path old_progress@VisitorTWorkerProgress{..} =
    (old_progress { workerNewSolutions = Seq.empty }
    ,VisitorWorkerStatusUpdate
        (Fold.toList workerNewSolutions)
        (computeRemainingWorkloadUsingProgress initial_path old_progress)
    )

constructWorkerStatusUpdateAndUpdateEnvironment ::
    VisitorTWorkerEnvironment m α →
    IO (VisitorWorkerStatusUpdate α)
constructWorkerStatusUpdateAndUpdateEnvironment VisitorTWorkerEnvironment{..} =
    atomicModifyIORef workerCurrentProgress $
        constructWorkerStatusUpdate workerInitialPath
-- @+node:gcross.20110923164140.1286: *3* forkWorkerThread
forkWorkerThread ::
    Visitor α →
    VisitorWorkload →
    IO (VisitorWorkerEnvironment α)
forkWorkerThread visitor VisitorWorkload{..} = do
    progress_ref ← newIORef $ VisitorTWorkerProgress
        {   workerFrozenCheckpointCursor = Seq.empty
        ,   workerContext = Seq.empty
        ,   workerRemainingCheckpoint = visitorWorkloadCheckpoint
        ,   workerNewSolutions = Seq.empty
        }
    termination_status_ivar ← IVar.new
    let initial_label = labelFromPath visitorWorkloadPath
        loop checkpoint visitor =
            (let (maybe_solution,context_update) = stepVisitorThroughCheckpoint checkpoint visitor
             in atomicModifyIORef progress_ref $ \(old_progress@VisitorTWorkerProgress{..}) →
                    let (new_context,new_checkpoint,maybe_next_loop) =
                            case applyContextUpdate context_update workerContext of
                                Nothing →
                                    (Seq.empty,Explored,Nothing)
                                Just (new_context,new_checkpoint,new_visitor) →
                                    (new_context,new_checkpoint,Just (new_checkpoint,new_visitor))
                    in (old_progress
                        {   workerContext = new_context
                        ,   workerRemainingCheckpoint = new_checkpoint
                        ,   workerNewSolutions =
                                maybe
                                    id
                                    (
                                        flip (|>)
                                        .
                                        VisitorSolution
                                        (
                                            applyContextToLabel workerContext
                                            .
                                            applyCheckpointCursorToLabel workerFrozenCheckpointCursor
                                            $
                                            initial_label
                                        )
                                    )
                                    maybe_solution
                                workerNewSolutions
                        }
                        ,maybe_next_loop
                        )
            )
            >>=
            maybe
                (IVar.write termination_status_ivar Nothing)
                (uncurry loop)
    thread_id ← forkIO $
        (loop visitorWorkloadCheckpoint
         .
         walkVisitorDownPath visitorWorkloadPath
         $
         visitor
        )
        `catch`
        (IVar.write termination_status_ivar . Just)
    return $
        VisitorTWorkerEnvironment
            visitorWorkloadPath
            thread_id
            progress_ref
            termination_status_ivar
-- @+node:gcross.20111004110500.1247: *3* runVisitorTWorkerRequestProcessor
runVisitorTWorkerRequestProcessor ::
    MonadCatchIO m ⇒
    Maybe Int →
    m () →
    m (Maybe (VisitorWorkerRequest m α)) →
    VisitorTWorkerEnvironment n α →
    m VisitorWorkerTerminationReason
runVisitorTWorkerRequestProcessor
    maybe_delay_between_polls
    notifyNewSolutions
    getNextRequest
    environment@VisitorTWorkerEnvironment{..}
  = loop
  where
    loop = do
        (liftIO . readIORef $ workerCurrentProgress)
        >>=
        ((return () ?? notifyNewSolutions) . Seq.null . workerNewSolutions)
        >>
        (liftIO . IVar.nonblocking . IVar.read $ workerTerminationStatus)
        >>=
        maybe
            (getNextRequest >>= processRequest)
            (return . maybe VisitorWorkerFinished VisitorWorkerFailed)

    wait = liftIO . maybe yield threadDelay $ maybe_delay_between_polls

    processRequest Nothing = wait >> loop
    processRequest (Just AbortWorker) =
        liftIO (killThread workerThreadId)
        >>
        return VisitorWorkerAborted
    processRequest (Just (StatusUpdateRequested submitStatusUpdate)) =
        liftIO (constructWorkerStatusUpdateAndUpdateEnvironment environment )
        >>=
        submitStatusUpdate
        >>
        loop
    processRequest (Just (WorkloadStealRequested submitWorkload notifyFailure)) =
        liftIO (stealWorkloadAndUpdateEnvironment environment)
        >>=
        maybe notifyFailure submitWorkload
        >>
        loop
-- @+node:gcross.20110923164140.1260: *3* stealWorkload(AndUpdateEnvironment)
stealWorkload ::
    VisitorPath →
    VisitorTWorkerProgress m α →
    (VisitorTWorkerProgress m α,Maybe VisitorWorkload)
stealWorkload initial_path old_progress@VisitorTWorkerProgress{..} = go workerContext Seq.empty
  where
    go (viewl → EmptyL) _ = (old_progress,Nothing)
    go (viewl → step :< rest_context) checkpoint_cursor_to_freeze = case step of
        BranchContextStep active_branch →
            go rest_context (checkpoint_cursor_to_freeze |> ChoiceCheckpointD active_branch Explored)
        CacheContextStep cache →
            go rest_context (checkpoint_cursor_to_freeze |> CacheCheckpointD cache)
        LeftChoiceContextStep other_checkpoint _ →
            let new_frozen_checkpoint_cursor =
                    (|> ChoiceCheckpointD RightBranchActive other_checkpoint)
                    .
                    (>< checkpoint_cursor_to_freeze)
                    $
                    workerFrozenCheckpointCursor
            in (old_progress
                    {   workerFrozenCheckpointCursor = new_frozen_checkpoint_cursor
                    ,   workerContext = rest_context
                    }
               ,Just $
                VisitorWorkload
                    (initial_path >< pathFromCursor new_frozen_checkpoint_cursor)
                    other_checkpoint
               )

stealWorkloadAndUpdateEnvironment :: VisitorTWorkerEnvironment m α → IO (Maybe VisitorWorkload)
stealWorkloadAndUpdateEnvironment VisitorTWorkerEnvironment{..} =
    atomicModifyIORef workerCurrentProgress $
        stealWorkload workerInitialPath
-- @-others
-- @-leo
