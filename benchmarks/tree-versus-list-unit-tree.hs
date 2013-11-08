{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main
import Data.Monoid
import System.Environment

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import LogicGrowsOnTrees.Parallel.Adapter.Threads (setNumberOfWorkers)
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

makeTree x = perfectTree x 2 15

main = defaultMain
    [bench "list" $ nf (mconcat . makeTree) ()
    ,bench "tree" $ nf (exploreTree . makeTree) ()
    ,bench "tree w/ checkpointing" $ nf (exploreTreeStartingFromCheckpoint Unexplored . makeTree) ()
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (makeTree ())
    ,bench "tree using single thread (direct)" $ Threads.exploreTree (setNumberOfWorkers 1) (makeTree ())
    ,bench "tree using single thread (main)" $
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                (const $ return ())
                (makeTree ())
    ]
