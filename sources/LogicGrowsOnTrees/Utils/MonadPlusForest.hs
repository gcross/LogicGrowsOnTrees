{-# LANGUAGE UnicodeSyntax #-}

{-|
This module implements an algorithm for building a tree from the bottom up, one
element at a time; it is based on a data structure that is a forest of perfectly
balanced binary trees that increase strictly in size (and might have gaps at
some sizes). The idea is that as each element is added to the forest, it first
is added to the forest as a height 1 tree; if there is another height 1 tree in
the forest then the two are `mplus`ed to form a height 2 tree; if there is
another height 2 tree then the two trees are `mplus`ed to form a height 3 tree,
and so on until there are no longer two trees in the forest with the same
height. When there are no more elements to add, the trees in the forest are
consolidated --- that is, the forest is folded, using `mplus` as the fold
function, starting with the smallest tree and ending with the largest tree.
Because every tree in the forest is a perfectly balanced tree, and the largest
tree has at least as many elements as the total number of elements in all other
trees in the forest, the end result is a search tree where at least half of the
elements are in a perfectly balanced subtree; this is often close enough to
being fully balanced tree.

From a programmer perspective, all you need to know is the following to apply
this algorithm: first you call 'emptyForest' to create an empty forest, then you
add each element using 'addToForest', and when you are done you consolidate the
forest using 'consolidateForest' to obtain the final tree.
 -}

module LogicGrowsOnTrees.Utils.MonadPlusForest
    ( ForestOfPerfectlyBalancedTrees
    , emptyForest
    , addToForest
    , consolidateForest
    ) where

import Control.Monad (MonadPlus(..))
import Data.Word (Word)

data PerfectlyBalancedTree α = PerfectlyBalancedTree {-# UNPACK #-} !Word α

{-| The forest of perfectly balanced trees (of 'MonadPlus') of strictly increasing size. -}
data ForestOfPerfectlyBalancedTrees α =
    EmptyForest
  | ForestWithPerfectlyBalancedTree {-# UNPACK #-} !Word α !(ForestOfPerfectlyBalancedTrees α)

{-| An empty forest. -}
emptyForest :: ForestOfPerfectlyBalancedTrees α
emptyForest = EmptyForest
{-# INLINE emptyForest #-}

{-| Adds a MonadPlus to a forest. -}
addToForest :: MonadPlus m ⇒ ForestOfPerfectlyBalancedTrees (m α) → m α → ForestOfPerfectlyBalancedTrees (m α)
addToForest forest x = go forest (PerfectlyBalancedTree 0 x)
  where
    go EmptyForest (PerfectlyBalancedTree i x) = ForestWithPerfectlyBalancedTree i x EmptyForest
    go forest@(ForestWithPerfectlyBalancedTree i' x' rest) (PerfectlyBalancedTree i x)
     | i == i' = go rest (PerfectlyBalancedTree (i+1) (x' `mplus` x))
     | otherwise = ForestWithPerfectlyBalancedTree i x forest
{-# INLINE addToForest #-}

{-| Consoldates the forest to form the resulting MonadPlus. -}
consolidateForest :: MonadPlus m ⇒ ForestOfPerfectlyBalancedTrees (m α) → m α
consolidateForest EmptyForest = mzero
consolidateForest (ForestWithPerfectlyBalancedTree _ x rest) = go rest x
  where
    go EmptyForest x = x
    go (ForestWithPerfectlyBalancedTree _ x' rest) x = go rest (x `mplus` x')
{-# INLINE consolidateForest #-}
