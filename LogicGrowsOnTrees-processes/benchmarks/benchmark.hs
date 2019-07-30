{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Exception (SomeException,catch)
import Criterion.Main
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
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

depth :: Word
depth = 15

main :: IO ()
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

benchmarks :: String → [Benchmark]
benchmarks worker_filepath =
    [bench "list" $ nf (getWordSum . mconcat . trivialPerfectTree 2) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . trivialPerfectTree 2) depth
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . trivialPerfectTree 2) depth
    ,bench "tree using worker" $ nfIO $
         exploreTreeGeneric AllMode Pure (trivialPerfectTree 2 depth)
    ,bench "tree using single thread" $ nfIO $
         Threads.exploreTree (setNumberOfWorkers 1) (trivialPerfectTree 2 depth)
    ,bench "tree using single process" $ nfIO $
        Processes.runSupervisor
            (AllMode :: ExplorationMode (AllMode WordSum))
            worker_filepath
            ["worker-bee"]
            (const $ return ())
            mempty
            (setNumberOfWorkers 1)
    ]
