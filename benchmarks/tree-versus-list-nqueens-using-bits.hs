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
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensUsingBitsCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensUsingBitsCount) n
    ,bench "tree w/ checkpointing" $
         nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensUsingBitsCount) n
    ,bench "tree using worker" $ nfIO (doWorker n)
    ]
  where
    n = 11

    -- This needs to be here because otherwise nqueensUsingBitsCount n only gets
    -- evaluated once, which distorts the benchmark.
    doWorker board_size = exploreTreeGeneric AllMode Pure (nqueensUsingBitsCount board_size)
    {-# NOINLINE doWorker #-}
