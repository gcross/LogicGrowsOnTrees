{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad
import Criterion.Main
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
  | otherwise  = leftLopsidedTree (depth-1) `mplus` return (WordSum 1)
{-# NOINLINE leftLopsidedTree #-}

rightLopsidedTree :: MonadPlus m ⇒ Word → m WordSum
rightLopsidedTree depth
  | depth == 0 = mzero
  | otherwise  = return (WordSum 1) `mplus` rightLopsidedTree (depth-1)
{-# NOINLINE rightLopsidedTree #-}

main :: IO ()
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
        [bench "left" $ nfIO (doWorker leftLopsidedTree depth)
        ,bench "right" $ nfIO (doWorker rightLopsidedTree depth)
        ]
    ,bgroup "tree using single thread (direct)"
        [bench "left" $ nfIO (doThreadDirect leftLopsidedTree depth)
        ,bench "right" $ nfIO (doThreadDirect rightLopsidedTree depth)
        ]
    ,bgroup "tree using single thread (main)"
        [bench "left" $ nfIO (doThreadMain leftLopsidedTree depth)
        ,bench "right" $ nfIO (doThreadMain leftLopsidedTree depth)
        ]
    ]
  where
    depth = 4096

    doWorker lopsidedTree tree_depth =
        exploreTreeGeneric AllMode Pure (lopsidedTree tree_depth :: Tree WordSum)
    {-# NOINLINE doWorker #-}

    doThreadDirect lopsidedTree tree_depth =
        Threads.exploreTree (setNumberOfWorkers 1) (lopsidedTree tree_depth :: Tree WordSum)
    {-# NOINLINE doThreadDirect #-}

    doThreadMain lopsidedTree tree_depth =
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                mempty
                (const $ pure ())
                (lopsidedTree tree_depth :: Tree WordSum)
    {-# NOINLINE doThreadMain #-}
