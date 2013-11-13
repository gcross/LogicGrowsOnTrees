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
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensWithNothingAtBottomCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensWithNothingAtBottomCount) n
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensWithNothingAtBottomCount) n
    ,bench "tree using worker" $ doWorker n
    ]
  where
    n = 12

    -- This needs to be here because otherwise nqueensWithNothingAtBottomCount n
    -- only gets evaluated once, which distorts the benchmark.
    doWorker n = exploreTreeGeneric AllMode Pure (nqueensWithNothingAtBottomCount n)
    {-# NOINLINE doWorker #-}
