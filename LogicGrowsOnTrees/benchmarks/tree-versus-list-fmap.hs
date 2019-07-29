{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad
import Criterion.Main

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

bindAndPlusIt :: (Functor m, MonadPlus m) ⇒ Int → m WordSum
bindAndPlusIt (-1) = mzero
bindAndPlusIt 0 = return (WordSum 1)
bindAndPlusIt depth = fmap (\(WordSum x) → WordSum (x+1)) $ bindAndPlusIt (depth-1) `mplus` bindAndPlusIt (depth-2)
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
