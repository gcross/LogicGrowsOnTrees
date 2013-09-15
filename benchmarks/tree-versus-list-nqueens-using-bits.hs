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
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensUsingBitsCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensUsingBitsCount) n
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensUsingBitsCount) n
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (nqueensUsingBitsCount n)
    ]
  where n = 11
