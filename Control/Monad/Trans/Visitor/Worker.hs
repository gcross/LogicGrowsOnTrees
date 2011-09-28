-- @+leo-ver=5-thin
-- @+node:gcross.20110923164140.1252: * @file Control/Monad/Trans/Visitor/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923164140.1253: ** << Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Worker where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923164140.1254: ** << Import needed modules >>
import Prelude hiding (catch)

import Control.Concurrent (killThread,threadDelay,ThreadId,yield)
import Control.Concurrent.Forkable (ForkableMonad(..))
import Control.Concurrent.MVar (MVar,newMVar,putMVar,takeMVar)
import Control.Exception (AsyncException(ThreadKilled),SomeException)
import Control.Monad.CatchIO (MonadCatchIO(catch),throw)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Loops

import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef,)
import Data.Sequence ((|>),(><),viewl,ViewL(..))
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
data VisitorWorkload = VisitorWorkload VisitorPath VisitorCheckpoint
-- @+node:gcross.20110923164140.1256: *3* VisitorWorkerRequest
data VisitorWorkerRequest m =
    CheckpointRequested (VisitorWorkload → m ())
  | WorkloadStealRequested (VisitorWorkload → m ()) (m ())
  | AbortRequested
-- @+node:gcross.20110923164140.1257: *3* VisitorTWorkerContext
data VisitorTWorkerContext m α = VisitorWorkerContext
    {   workerInitialPath :: VisitorPath
    ,   workerStealPrefixPath :: VisitorPath
    ,   workerPartialCheckpoint :: VisitorPartialCheckpoint
    ,   workerCurrentCheckpointContext :: VisitorTCheckpointContext m α
    }
type VisitorWorkerContext α = VisitorTWorkerContext Identity α
-- @+node:gcross.20110923164140.1264: *3* VisitorSolution
data VisitorSolution α = VisitorSolution VisitorPath α
-- @+node:gcross.20110923164140.1259: ** Functions
-- @+node:gcross.20110923164140.1263: *3* doWorkload
doWorkload ::
    (ForkableMonad m, MonadCatchIO m) ⇒
    Maybe Int →
    (VisitorSolution α → m ()) →
    m (Maybe (VisitorWorkerRequest m)) →
    (Maybe SomeException → m ()) →
    VisitorWorkload →
    Visitor α →
    m ()
doWorkload
    maybe_delay
    acceptSolution
    getNextRequest
    notifyFinished
    (VisitorWorkload initial_path checkpoint)
    visitor
    = do
    path_var :: MVar VisitorPath ← liftIO $ newMVar initial_path
    context_ref :: IORef (VisitorCheckpointContext α) ← liftIO $ newIORef Seq.empty
    finished_ref :: IORef (Maybe (Maybe SomeException)) ← liftIO $ newIORef Nothing
    worker_thread_id :: ThreadId ← forkIO $
        (
            (
                runVisitorThroughCheckpoint
                    (\result → 
                        (liftIO $ do
                            path ← takeMVar path_var
                            context ← readIORef context_ref
                            let result_path = path >< pathFromContext context
                            putMVar path_var path
                            return result_path
                        ) >>= acceptSolution . flip VisitorSolution result
                    )
                    (\move →
                        liftIO $
                        atomicModifyIORef context_ref $ \context →
                        case applyContextMove move context of
                            Nothing → (context,Nothing)
                            Just (new_context,checkpoint,visitor) → (new_context,Just (checkpoint,visitor))
                    )
                    checkpoint
                .
                walkVisitor initial_path
                $
                visitor
            )
            >>
            (liftIO $ writeIORef finished_ref (Just Nothing))
        ) `catch` (
            \(e :: AsyncException) →
                case e of
                    ThreadKilled → return ()
                    _ → throw e
        ) `catch` (
            liftIO . writeIORef finished_ref . Just . Just
        )
    let wait = liftIO $ maybe yield threadDelay maybe_delay
        loop partial_checkpoint = do
            finished ← liftIO $ readIORef finished_ref
            case finished of
                Just maybe_exception → return maybe_exception
                Nothing → do
                    request ← getNextRequest
                    case request of
                        Nothing → wait >> loop partial_checkpoint
                        Just (CheckpointRequested submitCheckpoint) →
                            liftIO (readIORef context_ref)
                            >>=
                            (
                                submitCheckpoint
                                .
                                VisitorWorkload initial_path
                                .
                                partial_checkpoint
                                .
                                checkpointFromContext
                            )
                            >>
                            loop partial_checkpoint
                        Just (WorkloadStealRequested submitWorkload notifyWorkloadNotStealableNow) → do
                            (maybe_workload_checkpoint,workload_path,new_partial_checkpoint) ← liftIO $ do
                                path ← takeMVar path_var
                                (maybe_workload_checkpoint,path_suffix,partial_checkpoint_suffix) ←
                                    atomicModifyIORef context_ref stealWorkloadIfPossible
                                let new_path = path >< path_suffix
                                putMVar path_var new_path
                                let new_partial_checkpoint = partial_checkpoint . partial_checkpoint_suffix
                                return (maybe_workload_checkpoint,new_path,new_partial_checkpoint)
                            case maybe_workload_checkpoint of
                                Nothing →
                                    notifyWorkloadNotStealableNow >> wait
                                Just workload_checkpoint →
                                    submitWorkload (VisitorWorkload workload_path workload_checkpoint)
                            loop new_partial_checkpoint
                        Just AbortRequested → liftIO $ do
                            killThread worker_thread_id
                            return Nothing
    loop id >>= notifyFinished
-- @+node:gcross.20110923164140.1260: *3* stealWorkloadIfPossible
stealWorkloadIfPossible ::
    VisitorTCheckpointContext m α →
    (VisitorTCheckpointContext m α,(Maybe VisitorCheckpoint,VisitorPath,VisitorCheckpoint → VisitorCheckpoint))
stealWorkloadIfPossible = go Seq.empty id
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
-- @-others
-- @-leo
