{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main
import Data.Monoid

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Examples.Queens
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

main = defaultMain
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensWithListAtBottomCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensWithListAtBottomCount) n
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensWithListAtBottomCount) n
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (nqueensWithListAtBottomCount n)
    ]
  where n = 12
