{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main
import Data.Monoid
import System.Environment

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum
import qualified LogicGrowsOnTrees.Parallel.Adapter.Processes as Processes
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import LogicGrowsOnTrees.Parallel.Adapter.Threads (setNumberOfWorkers)
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialPerfectTree 2) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . trivialPerfectTree 2) depth
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . trivialPerfectTree 2) depth
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (trivialPerfectTree 2 depth)
    ,bench "tree using single thread (direct)" $ Threads.exploreTree (setNumberOfWorkers 1) (trivialPerfectTree 2 depth)
    ,bench "tree using single thread (main)" $
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                (const $ return ())
                (trivialPerfectTree 2 depth)
    ,bench "tree using single process (direct)" $
        Processes.runExplorer
            (const AllMode)
            Pure
            (return ((),()))
            (const $ return ())
            (const $ trivialPerfectTree 2 depth)
            (\_ _ → return mempty)
            (\_ _ → setNumberOfWorkers 1)
    ,bench "tree using single process (main)" $
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Processes.driver
                (const $ return ())
                (trivialPerfectTree 2 depth)
    ]
  where depth = 15
