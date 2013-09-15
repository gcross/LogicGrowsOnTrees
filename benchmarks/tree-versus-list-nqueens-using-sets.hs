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
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensUsingSetsCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensUsingSetsCount) n
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensUsingSetsCount) n
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (nqueensUsingSetsCount n)
    ]
  where n = 11
