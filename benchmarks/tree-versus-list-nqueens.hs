-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main
import Data.Monoid

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Examples.Queens
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Common.Worker (Purity(Pure),exploreTreeGeneric)
-- }}}

main = defaultMain
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensCount) n
    ,bench "tree" $ nf (getWordSum . exploreTree . nqueensCount) n
    ,bench "tree w/ checkpointing" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . nqueensCount) n
    ,bench "tree using worker" $ exploreTreeGeneric AllMode Pure (nqueensCount n)
    ]
  where n = 13
