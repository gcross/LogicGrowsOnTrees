{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad
import Criterion.Main
import Data.List (genericReplicate)
import Data.Monoid
import Data.Word
import System.Environment

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.WordSum
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import LogicGrowsOnTrees.Parallel.Adapter.Threads (setNumberOfWorkers)
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

bindAndPlusIt :: (Functor m, MonadPlus m) ⇒ Int → m WordSum
bindAndPlusIt (-1) = mzero
bindAndPlusIt 0 = return (WordSum 1)
bindAndPlusIt depth = fmap (\(WordSum x) → WordSum (x+1)) $ bindAndPlusIt (depth-1) `mplus` bindAndPlusIt (depth-2)
{-# NOINLINE bindAndPlusIt #-}

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . bindAndPlusIt) depth
    ,bench "tree"$ nf (getWordSum . exploreTree . bindAndPlusIt) depth
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . bindAndPlusIt) depth
    ,bench "tree using worker" $ doWorker bindAndPlusIt depth
    ]
  where
    depth = 20

    doWorker lopsidedTree depth = exploreTreeGeneric AllMode Pure (lopsidedTree depth :: Tree WordSum)
    {-# NOINLINE doWorker #-}
