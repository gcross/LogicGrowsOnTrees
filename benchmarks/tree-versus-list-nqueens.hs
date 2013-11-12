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
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensCount) n
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensCount) n
    ,bench "tree using worker" $ doWorker n
    ]
  where
    n = 13

    -- This needs to be here because otherwise nqueensCount n only gets
    -- evaluated once, which distorts the benchmark.
    doWorker n = exploreTreeGeneric AllMode Pure (nqueensCount n)
    {-# NOINLINE doWorker #-}
