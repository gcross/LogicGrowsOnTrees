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

nullPerfectTree :: MonadPlus m ⇒ Word → m WordSum
nullPerfectTree depth
  | depth == 0 = mzero
  | otherwise  = nullPerfectTree (depth-1) `mplus` nullPerfectTree (depth-1)
{-# NOINLINE nullPerfectTree #-}

main :: IO ()
main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . nullPerfectTree) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . nullPerfectTree) depth
    ,bench "tree w/ checkpointing" $
         nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nullPerfectTree) depth
    ,bench "tree using worker" $ nfIO (doWorker depth)
    ,bench "tree using single thread (direct)" $ nfIO (doThreadDirect depth)
    ,bench "tree using single thread (main)" $ nfIO (doThreadMain depth)
    ]
  where
    depth = 15

    -- These need to be here because otherwise nullPerfectTree depth only gets
    -- evaluated once, which distorts the benchmark.

    doWorker tree_depth =
        exploreTreeGeneric AllMode Pure (nullPerfectTree tree_depth :: Tree WordSum)
    {-# NOINLINE doWorker #-}

    doThreadDirect tree_depth =
        Threads.exploreTree (setNumberOfWorkers 1) (nullPerfectTree tree_depth :: Tree WordSum)
    {-# NOINLINE doThreadDirect #-}

    doThreadMain tree_depth =
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                mempty
                (const $ pure ())
                (nullPerfectTree tree_depth :: Tree WordSum)
    {-# NOINLINE doThreadMain #-}

