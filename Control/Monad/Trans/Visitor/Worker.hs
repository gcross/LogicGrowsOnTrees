-- @+leo-ver=5-thin
-- @+node:gcross.20110923164140.1252: * @file Control/Monad/Trans/Visitor/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923164140.1253: ** << Language extensions >>
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Concurrent (killThread,threadDelay,ThreadId,yield)
import Control.Concurrent.Forkable (ForkableMonad(..))
import Control.Concurrent.STM (STM,atomically)
import Control.Concurrent.STM.TVar (TVar,newTVarIO,readTVar,writeTVar)
import Control.Concurrent.MVar (MVar,newEmptyMVar,putMVar,takeMVar,tryTakeMVar)
import Control.Exception (AsyncException(ThreadKilled),SomeException,evaluate)
import Control.Monad (liftM2,liftM3)
import Control.Monad.CatchIO (finally,MonadCatchIO(catch),throw)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops

import Data.Foldable as Fold
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef,)
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
-- @+node:gcross.20110923164140.1261: *3* VisitorWorkload
data VisitorWorkload = VisitorWorkload
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: VisitorCheckpoint
    }
-- @+node:gcross.20110923164140.1257: *3* VisitorTWorkerEnvironment
data VisitorTWorkerEnvironment m α = VisitorTWorkerEnvironment
    {   workerInitialPath :: VisitorPath
    ,   workerCurrentPathVar :: TVar VisitorPath
    ,   workerCurrentPartialCheckpointVar :: TVar VisitorPartialCheckpoint
    ,   workerCurrentCheckpointContextVar :: TVar (VisitorTCheckpointContext m α)
    ,   workerSolutionsFoundVar :: TVar (Seq (VisitorSolution α))
    ,   workerFinishedSignal :: MVar (Maybe SomeException)
    }
type VisitorWorkerEnvironment α = VisitorTWorkerEnvironment Identity α
-- @+node:gcross.20110923164140.1264: *3* VisitorSolution
data VisitorSolution α = VisitorSolution VisitorPath α
-- @+node:gcross.20111004110500.1246: *3* VisitorWorkerRequest
data VisitorWorkerRequest m =
    CheckpointRequested (VisitorWorkload → m ())
  | WorkloadStealRequested (VisitorWorkload → m ()) (m ())
-- @+node:gcross.20110923164140.1259: ** Functions
-- @+node:gcross.20110923164140.1287: *3* computeRemainingWorkload(UsingEnvironment)
computeRemainingWorkload ::
    VisitorPath →
    VisitorPartialCheckpoint →
    VisitorTCheckpointContext m α →
    VisitorWorkload
computeRemainingWorkload initial_path partial_checkpoint =
    VisitorWorkload initial_path
    .
    partial_checkpoint
    .
    checkpointFromContext

computeRemainingWorkloadUsingEnvironmentWithinTransaction ::
    VisitorTWorkerEnvironment n α →
    STM VisitorWorkload
computeRemainingWorkloadUsingEnvironmentWithinTransaction =
    liftM3 computeRemainingWorkload
        <$> return . workerInitialPath
        <*> readTVar . workerCurrentPartialCheckpointVar
        <*> readTVar . workerCurrentCheckpointContextVar

computeRemainingWorkloadUsingEnvironment ::
    MonadIO m ⇒
    VisitorTWorkerEnvironment n α →
    m VisitorWorkload
computeRemainingWorkloadUsingEnvironment =
    liftIO
    .
    atomically
    .
    computeRemainingWorkloadUsingEnvironmentWithinTransaction
-- @+node:gcross.20110923164140.1286: *3* forkWorkerThread(UsingEnvironment)
forkWorkerThread ::
    Visitor α →
    VisitorWorkload →
    TVar VisitorPath →
    TVar (VisitorCheckpointContext α) →
    TVar (Seq (VisitorSolution α)) →
    MVar (Maybe SomeException) →
    IO ThreadId
forkWorkerThread
    visitor
    VisitorWorkload{..}
    path_var
    context_var
    solutions_var
    finished_signal
    = forkIO $
    (
        (
            runVisitorThroughCheckpoint
                (\result → atomically $ do
                    path ← readTVar path_var
                    context ← readTVar context_var
                    let solution_path = path >< pathFromContext context
                    readTVar solutions_var >>=
                        writeTVar solutions_var . (|> VisitorSolution solution_path result)
                )
                (\move → atomically $ do
                    old_context ← readTVar context_var
                    case applyContextMove move old_context of
                        Nothing → return Nothing
                        Just (new_context,checkpoint,visitor) → do
                            writeTVar context_var new_context
                            return $ Just (checkpoint,visitor)
                )
                visitorWorkloadCheckpoint
            .
            walkVisitor visitorWorkloadPath
            $
            visitor
        )
        >>
        putMVar finished_signal Nothing
    ) `catch` (
        putMVar finished_signal . Just
    )

forkWorkerThreadUsingEnvironment ::
    Visitor α →
    VisitorWorkload →
    VisitorWorkerEnvironment α →
    IO ThreadId
forkWorkerThreadUsingEnvironment visitor workload =
    forkWorkerThread visitor workload
        <$> workerCurrentPathVar
        <*> workerCurrentCheckpointContextVar
        <*> workerSolutionsFoundVar
        <*> workerFinishedSignal
-- @+node:gcross.20110923164140.1291: *3* initializeVisitorWorkerEnvironment
initializeVisitorTWorkerEnvironment ::
    VisitorPath →
    IO (VisitorTWorkerEnvironment m α)
initializeVisitorTWorkerEnvironment initial_path =
    VisitorTWorkerEnvironment initial_path
        <$> newTVarIO initial_path
        <*> newTVarIO id
        <*> newTVarIO Seq.empty
        <*> newTVarIO Seq.empty
        <*> newEmptyMVar
-- @+node:gcross.20111004110500.1247: *3* runVisitorTWorkerRequestProcessor
runVisitorTWorkerRequestProcessor ::
    MonadCatchIO m ⇒
    Maybe Int →
    (VisitorWorkload → [VisitorSolution α] → m ()) →
    m (Maybe (VisitorWorkerRequest m)) →
    VisitorTWorkerEnvironment n α →
    m ()
runVisitorTWorkerRequestProcessor
    maybe_delay
    updateCheckpoint
    getNextRequest
    environment@VisitorTWorkerEnvironment{workerFinishedSignal,workerSolutionsFoundVar}
  = loop
  where
    loop =
        (liftIO . atomically $ do
            solutions ← readTVar workerSolutionsFoundVar
            if Seq.null solutions
                then return Nothing
                else do
                    writeTVar workerSolutionsFoundVar Seq.empty
                    checkpoint ← computeRemainingWorkloadUsingEnvironmentWithinTransaction environment
                    return $ Just (checkpoint,Fold.toList solutions)
        )
        >>=
        (maybe (return ()) (uncurry updateCheckpoint))
        >>
        liftIO (tryTakeMVar workerFinishedSignal)
        >>=
        maybe
            (getNextRequest >>= processRequest >> loop)
            (maybe
                (return ())
                throw
            )

    wait = liftIO . maybe yield threadDelay $ maybe_delay

    processRequest Nothing = wait
    processRequest (Just (CheckpointRequested submitCheckpoint)) =
        (liftIO . atomically $ do
            solutions ← readTVar workerSolutionsFoundVar
            writeTVar workerSolutionsFoundVar Seq.empty
            checkpoint ← computeRemainingWorkloadUsingEnvironmentWithinTransaction environment
            return (checkpoint,Fold.toList solutions)
        )
        >>=
        (uncurry updateCheckpoint)
    processRequest (Just (WorkloadStealRequested submitWorkload notifyFailure)) =
        stealWorkloadAndUpdateEnvironment environment
        >>=
        maybe notifyFailure submitWorkload
-- @+node:gcross.20110923164140.1260: *3* stealWorkload(AndUpdateEnvironment)
stealWorkload ::
    VisitorTCheckpointContext m α →
    (VisitorTCheckpointContext m α,(Maybe VisitorCheckpoint,VisitorPath,VisitorCheckpoint → VisitorCheckpoint))
stealWorkload = go Seq.empty id
  where
    go steal_prefix_path partial_checkpoint context = case viewl context of
        EmptyL →
            (Seq.empty
            ,(Nothing
             ,steal_prefix_path
             ,partial_checkpoint
             )
            )
        BranchCheckpointD which_explored :< rest_context →
            go (steal_prefix_path |> ChoiceStep (not which_explored))
               (partial_checkpoint . BranchCheckpoint (not which_explored))
               rest_context
        CacheCheckpointD cache :< rest_context →
            go (steal_prefix_path |> CacheStep cache)
               (partial_checkpoint . CacheCheckpoint cache)
               rest_context
        ChoiceCheckpointD which_explored other_checkpoint _ :< rest_context →
            (rest_context
            ,(Just other_checkpoint
             ,steal_prefix_path |> ChoiceStep (not which_explored)
             ,partial_checkpoint . BranchCheckpoint (not which_explored)
             )
            )

stealWorkloadAndUpdateEnvironment ::
    MonadIO m ⇒
    VisitorTWorkerEnvironment n α →
    m (Maybe VisitorWorkload)
stealWorkloadAndUpdateEnvironment VisitorTWorkerEnvironment{..} = liftIO . atomically $ do
    (new_context,(maybe_workload_checkpoint,path_suffix,partial_checkpoint_suffix)) ←
        fmap stealWorkload (readTVar workerCurrentCheckpointContextVar)
    new_path ← fmap (>< path_suffix) (readTVar workerCurrentPathVar)
    writeTVar workerCurrentPathVar new_path
    readTVar workerCurrentPartialCheckpointVar >>=
        writeTVar workerCurrentPartialCheckpointVar . (. partial_checkpoint_suffix)
    return . fmap (VisitorWorkload new_path) $ maybe_workload_checkpoint
-- @-others
-- @-leo
