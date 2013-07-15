-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main
import Data.Monoid

import Visitor
import Visitor.Checkpoint
import Visitor.Utils.PerfectTree
import Visitor.Utils.WordSum
import Visitor.Parallel.Common.VisitorMode (VisitorMode(AllMode))
import Visitor.Parallel.Common.Worker (Purity(Pure),visitTreeGeneric)
-- }}}

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialPerfectTree 2) depth
    ,bench "tree generator" $ nf (getWordSum . visitTree . trivialPerfectTree 2) depth
    ,bench "tree generator w/ checkpointing" $ nf (getWordSum . visitTreeStartingFromCheckpoint Unexplored . trivialPerfectTree 2) depth
    ,bench "tree generator using worker" $ visitTreeGeneric AllMode Pure (trivialPerfectTree 2 depth)
    ]
  where depth = 15
