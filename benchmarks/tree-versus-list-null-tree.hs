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

import Debug.Trace

nullPerfectTree :: MonadPlus m ⇒ Word → m WordSum
nullPerfectTree depth
  | depth == 0 = mzero
  | otherwise  = nullPerfectTree (depth-1) `mplus` nullPerfectTree (depth-1)
{-# NOINLINE nullPerfectTree #-}

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . nullPerfectTree) depth
    ,bench "tree" $ nf (getWordSum . exploreTree . nullPerfectTree) depth
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nullPerfectTree) depth
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (nullPerfectTree depth :: Tree WordSum)
    ,bench "tree using single thread (direct)" $ Threads.exploreTree (setNumberOfWorkers 1) (nullPerfectTree depth :: Tree WordSum)
    ,bench "tree using single thread (main)" $
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                (const $ return ())
                (nullPerfectTree depth :: Tree WordSum)
    ]
  where depth = 15
