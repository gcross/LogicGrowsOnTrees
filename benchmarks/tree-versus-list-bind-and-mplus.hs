{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad
import Criterion.Main

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

bindAndPlusIt :: MonadPlus m ⇒ Int → m WordSum
bindAndPlusIt (-1) = mzero
bindAndPlusIt 0 = return (WordSum 1)
bindAndPlusIt depth = (return (depth-1) >>= bindAndPlusIt) `mplus` (return (depth-2) >>= bindAndPlusIt)
{-# NOINLINE bindAndPlusIt #-}

main :: IO ()
main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . bindAndPlusIt) depth
    ,bench "tree"$ nf (getWordSum . exploreTree . bindAndPlusIt) depth
    ,bench "tree w/ checkpointing" $
         nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . bindAndPlusIt) depth
    ,bench "tree using worker" $ nfIO (doWorker bindAndPlusIt depth)
    ]
  where
    depth = 20

    doWorker lopsidedTree tree_depth =
        exploreTreeGeneric AllMode Pure (lopsidedTree tree_depth :: Tree WordSum)
    {-# NOINLINE doWorker #-}
