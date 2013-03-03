-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Visitors.RoseTree where

-- Imports {{{
import Control.Monad (MonadPlus(mplus),msum)

import Data.Monoid (Monoid)
import Data.Semiring (Semiring(..))
import Data.Tree (Tree(..))
import Data.Word (Word)
-- }}}

-- Functions {{{

sumOverAllNodes :: -- {{{
    MonadPlus m ⇒
    Tree α →
    m α
sumOverAllNodes = sumOverAllNodesUsingBalancer msum
-- }}}

sumOverAllNodesUsingBalancer :: -- {{{
    MonadPlus m ⇒
    ([m α] → m α) →
    Tree α →
    m α
sumOverAllNodesUsingBalancer balance = go
  where
    go Node{..} = return rootLabel `mplus` (balance . map go $ subForest)
-- }}}

sumOverAllPathsToLeaves :: -- {{{
    ( MonadPlus m
    , Semiring α
    ) ⇒
    Tree α →
    m α
sumOverAllPathsToLeaves = sumOverAllPathsToLeavesUsingBalancer msum
-- }}}

sumOverAllPathsToLeavesUsingBalancer :: -- {{{
    ( MonadPlus m
    , Semiring α
    ) ⇒
    ([m α] → m α) →
    Tree α →
    m α
sumOverAllPathsToLeavesUsingBalancer balance = go munit
  where
    go path_value Node{..} =
        if null subForest
            then return new_value
            else balance . map (go new_value) $ subForest
      where
        new_value = path_value `mtimes` rootLabel
-- }}}

-- }}}
