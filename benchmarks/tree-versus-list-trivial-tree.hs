{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main
import System.Environment

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.PerfectTree (trivialPerfectTree)
import LogicGrowsOnTrees.Utils.WordSum
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import LogicGrowsOnTrees.Parallel.Adapter.Threads (setNumberOfWorkers)
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

main :: IO ()
main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialPerfectTree 2) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . trivialPerfectTree 2) depth
    ,bench "tree w/ checkpointing" $
         nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . trivialPerfectTree 2) depth
    ,bench "tree using worker" $ nfIO $
         exploreTreeGeneric AllMode Pure (trivialPerfectTree 2 depth)
    ,bench "tree using single thread (direct)" $ nfIO $
         Threads.exploreTree (setNumberOfWorkers 1) (trivialPerfectTree 2 depth)
    ,bench "tree using single thread (main)" $ nfIO $
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                mempty
                (const $ return ())
                (trivialPerfectTree 2 depth)
    ]
  where depth = 15
