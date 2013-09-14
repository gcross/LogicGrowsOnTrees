{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main
import Data.Monoid

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialPerfectTree 2) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . trivialPerfectTree 2) depth
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . trivialPerfectTree 2) depth
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (trivialPerfectTree 2 depth)
    ]
  where depth = 15
