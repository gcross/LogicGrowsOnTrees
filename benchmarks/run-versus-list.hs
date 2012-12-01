-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Monad
import Criterion.Main
import Data.Monoid

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
-- }}}

sumtree :: MonadPlus m ⇒ Int → m (Sum Int)
sumtree 0 = return (Sum 1)
sumtree n = sumtree (n-1) `mplus` sumtree (n-1)
{-# SPECIALIZE sumtree :: Int → [Sum Int] #-}
{-# SPECIALIZE sumtree :: Int → Visitor (Sum Int) #-}

main = defaultMain
    [bench "list" $ nf (getSum . mconcat . sumtree) 15
    ,bench "visitor" $ nf (getSum . runVisitor . sumtree) 15
    ,bench "visitor w/ checkpointing" $ nf (getSum . runVisitorThroughCheckpoint Unexplored . sumtree) 15
    ]