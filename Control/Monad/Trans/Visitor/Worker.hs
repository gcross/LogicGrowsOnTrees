-- @+leo-ver=5-thin
-- @+node:gcross.20110923164140.1252: * @file Control/Monad/Trans/Visitor/Worker.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923164140.1253: ** << Language extensions >>
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

import Control.Applicative (liftA2,liftA3)
import Control.Concurrent (killThread,threadDelay,ThreadId,yield)
import Control.Concurrent.Forkable (ForkableMonad(..))
import Control.Concurrent.MVar (MVar,newMVar,putMVar,takeMVar)
import Control.Exception (AsyncException(ThreadKilled),SomeException)
import Control.Monad (liftM3,liftM4)
import Control.Monad.CatchIO (finally,MonadCatchIO(catch),throw)
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
data VisitorWorkload = VisitorWorkload
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: VisitorCheckpoint
    }
-- @+node:gcross.20110923164140.1257: *3* VisitorTWorkerEnvironment
data VisitorTWorkerEnvironment m α = VisitorTWorkerEnvironment
    {   workerInitialPath :: VisitorPath
    ,   workerCurrentPathVar :: MVar VisitorPath
    ,   workerCurrentPartialCheckpointRef :: IORef VisitorPartialCheckpoint
    ,   workerCurrentCheckpointContextRef :: IORef (VisitorTCheckpointContext m α)
    ,   workerFinishedRef :: IORef (Maybe (Maybe SomeException))
    }
type VisitorWorkerEnvironment α = VisitorTWorkerEnvironment Identity α
-- @+node:gcross.20110923164140.1264: *3* VisitorSolution
data VisitorSolution α = VisitorSolution VisitorPath α
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

computeRemainingWorkloadUsingEnvironment ::
    VisitorTWorkerEnvironment m α →
    IO VisitorWorkload
computeRemainingWorkloadUsingEnvironment =
    (liftA3 . liftM3 $ computeRemainingWorkload)
        (return . workerInitialPath)
        (readIORef . workerCurrentPartialCheckpointRef)
        (readIORef . workerCurrentCheckpointContextRef)
-- @+node:gcross.20110923164140.1286: *3* forkWorkerThread(UsingEnvironment)
forkWorkerThread ::
    (α → IO β) →
    Visitor α →
    VisitorWorkload →
    IORef (VisitorCheckpointContext α) →
    IORef (Maybe (Maybe SomeException)) →
    IO ThreadId
forkWorkerThread acceptSolution visitor VisitorWorkload{..} context_ref finished_ref = forkIO $
    (
        (
            runVisitorThroughCheckpoint
                acceptSolution
                (\move →
                    atomicModifyIORef context_ref $ \context →
                    case applyContextMove move context of
                        Nothing → (context,Nothing)
                        Just (new_context,checkpoint,visitor) → (new_context,Just (checkpoint,visitor))
                )
                visitorWorkloadCheckpoint
            .
            walkVisitor visitorWorkloadPath
            $
            visitor
        )
        >>
        writeIORef finished_ref (Just Nothing)
    ) `catch` (
        writeIORef finished_ref . Just . Just
    )

forkWorkerThreadUsingEnvironment ::
    (α → IO β) →
    Visitor α →
    VisitorWorkload →
    VisitorWorkerEnvironment α →
    IO ThreadId
forkWorkerThreadUsingEnvironment acceptSolution visitor workload =
    liftA2 (forkWorkerThread acceptSolution visitor workload)
        workerCurrentCheckpointContextRef
        workerFinishedRef
-- @+node:gcross.20110923164140.1291: *3* initializeVisitorWorkerEnvironment
initializeVisitorTWorkerEnvironment ::
    VisitorPath →
    IO (VisitorTWorkerEnvironment m α)
initializeVisitorTWorkerEnvironment initial_path =
    liftM4 (VisitorTWorkerEnvironment initial_path)
        (newMVar initial_path)
        (newIORef id)
        (newIORef Seq.empty)
        (newIORef Nothing)
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
    VisitorTWorkerEnvironment m α →
    IO (Maybe VisitorWorkload)
stealWorkloadAndUpdateEnvironment VisitorTWorkerEnvironment{..} = do
    path ← takeMVar workerCurrentPathVar
    (maybe_workload_checkpoint,path_suffix,partial_checkpoint_suffix) ←
        atomicModifyIORef workerCurrentCheckpointContextRef stealWorkload
    let new_path = path >< path_suffix
    putMVar workerCurrentPathVar new_path
    atomicModifyIORef workerCurrentPartialCheckpointRef ((,()) . (. partial_checkpoint_suffix))
    return . fmap (VisitorWorkload new_path) $ maybe_workload_checkpoint
-- @-others
-- @-leo
