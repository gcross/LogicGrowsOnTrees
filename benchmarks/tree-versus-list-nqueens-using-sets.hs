{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Examples.Queens
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

main :: IO ()
main = defaultMain
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensUsingSetsCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensUsingSetsCount) n
    ,bench "tree w/ checkpointing" $
         nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensUsingSetsCount) n
    ,bench "tree using worker" $ nfIO (doWorker n)
    ]
  where
    n = 11

    -- This needs to be here because otherwise nqueensUsingSetsCount n only gets
    -- evaluated once, which distorts the benchmark.
    doWorker board_size = exploreTreeGeneric AllMode Pure (nqueensUsingSetsCount board_size)
    {-# NOINLINE doWorker #-}
