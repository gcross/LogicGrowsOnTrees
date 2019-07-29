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
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensWithListAtBottomCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensWithListAtBottomCount) n
    ,bench "tree w/ checkpointing" $
         nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensWithListAtBottomCount) n
    ,bench "tree using worker" $ nfIO (doWorker n)
    ]
  where
    n = 12

    -- This needs to be here because otherwise nqueensWithListAtBottomCount n
    -- only gets evaluated once, which distorts the benchmark.
    doWorker board_size = exploreTreeGeneric AllMode Pure (nqueensWithListAtBottomCount board_size)
    {-# NOINLINE doWorker #-}
