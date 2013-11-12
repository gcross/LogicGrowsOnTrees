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

leftLopsidedTree :: MonadPlus m ⇒ Word → m WordSum
leftLopsidedTree depth
  | depth == 0 = mzero
  | otherwise  = leftLopsidedTree (depth-1) `mplus` mzero
{-# NOINLINE leftLopsidedTree #-}

rightLopsidedTree :: MonadPlus m ⇒ Word → m WordSum
rightLopsidedTree depth
  | depth == 0 = mzero
  | otherwise  = mzero `mplus` rightLopsidedTree (depth-1)
{-# NOINLINE rightLopsidedTree #-}

main = defaultMain
    [bgroup "list"
        [bench "left" $ nf (getWordSum . mconcat . leftLopsidedTree) depth
        ,bench "right" $ nf (getWordSum . mconcat . rightLopsidedTree) depth
        ]
    ,bgroup "tree"
        [bench "left" $ nf (getWordSum . exploreTree . leftLopsidedTree) depth
        ,bench "right" $ nf (getWordSum . exploreTree . rightLopsidedTree) depth
        ]
    ,bgroup "tree w/ checkpointing"
        [bench "left" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . leftLopsidedTree) depth
        ,bench "right" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . rightLopsidedTree) depth
        ]
    ,bgroup "tree using worker"
        [bench "left" $ doWorker leftLopsidedTree depth
        ,bench "right" $ doWorker rightLopsidedTree depth
        ]
    ]
  where
    depth = 4096

    doWorker lopsidedTree depth = exploreTreeGeneric AllMode Pure (lopsidedTree depth :: Tree WordSum)
    {-# NOINLINE doWorker #-}
