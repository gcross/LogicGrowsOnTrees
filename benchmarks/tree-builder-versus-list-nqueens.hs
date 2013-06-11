-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main
import Data.Monoid

import Visitor
import Visitor.Checkpoint
import Visitor.Examples.Queens
import Visitor.Utils.WordSum
import qualified Visitor.Parallel.Common.Worker as Worker
-- }}}

main = defaultMain
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensCount) n
    ,bench "tree builder" $ nf (getWordSum . visitTree . nqueensCount) n
    ,bench "tree builder w/ checkpointing" $ nf (getWordSum . visitTreeStartingFromCheckpoint Unexplored . nqueensCount) n
    ,bench "tree builder using worker" $ Worker.visitTree (nqueensCount n)
    ]
  where n = 13
