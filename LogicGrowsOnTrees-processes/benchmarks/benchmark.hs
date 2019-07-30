{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude hiding (catch)

import Control.Exception (SomeException,catch)
import Criterion.Main
import Data.Monoid
import System.Environment
import System.IO

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.PerfectTree (trivialPerfectTree)
import LogicGrowsOnTrees.Utils.WordSum
import qualified LogicGrowsOnTrees.Parallel.Adapter.Processes as Processes
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import LogicGrowsOnTrees.Parallel.Adapter.Threads (setNumberOfWorkers)
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (AllMode,ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

depth = 15

main = do
    args ← getArgs
    case args of
        ["worker-bee"] →
            Processes.runWorkerUsingHandles
                AllMode
                Pure
                (trivialPerfectTree 2 depth)
                stdin
                stdout
             `catch`
             (\(e::SomeException) → error $ "Worker process failed: " ++ show e)
        _ → Processes.getProgFilepath >>= defaultMain . benchmarks

benchmarks worker_filepath =
    [bench "list" $ nf (getWordSum . mconcat . trivialPerfectTree 2) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . trivialPerfectTree 2) depth
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . trivialPerfectTree 2) depth
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (trivialPerfectTree 2 depth)
    ,bench "tree using single thread" $ Threads.exploreTree (setNumberOfWorkers 1) (trivialPerfectTree 2 depth)
    ,bench "tree using single process" $
        Processes.runSupervisor
            (AllMode :: ExplorationMode (AllMode WordSum))
            worker_filepath
            ["worker-bee"]
            (const $ return ())
            mempty
            (setNumberOfWorkers 1)
    ]
