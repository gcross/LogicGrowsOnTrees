{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main
import Control.DeepSeq
import Data.Monoid
import Data.Serialize
import System.Environment

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import LogicGrowsOnTrees.Parallel.Adapter.Threads (setNumberOfWorkers)
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

makeTree x = perfectTree x 2 15

-- We need to define our own version of () because the mconcat method for ()
-- completely ignores the argument list and so bypasses the cost of adding the
-- ()'s up.
data MyUnit = MyUnit

instance Serialize MyUnit where
    put _ = put ()
    get = return MyUnit

instance Monoid MyUnit where
    mempty = MyUnit
    mappend x y = x `seq` y `seq` MyUnit
    mconcat [] = MyUnit
    mconcat (x:xs) = x `seq` mconcat xs

instance NFData MyUnit where
    rnf x = x `seq` ()

main = defaultMain
    [bench "list" $ nf (mconcat . makeTree) MyUnit
    ,bench "tree" $ nf (exploreTree . makeTree) MyUnit
    ,bench "tree w/ checkpointing" $ nf (exploreTreeStartingFromCheckpoint Unexplored . makeTree) MyUnit
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (makeTree MyUnit)
    ,bench "tree using single thread (direct)" $ Threads.exploreTree (setNumberOfWorkers 1) (makeTree MyUnit)
    ,bench "tree using single thread (main)" $
        withArgs ["-n1"] $
            simpleMainForExploreTree
                Threads.driver
                (const $ return ())
                (makeTree MyUnit)
    ]
