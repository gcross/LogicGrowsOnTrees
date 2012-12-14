-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import qualified Data.IntSet as IntSet

import Control.Monad.Trans.Visitor (Visitor,allFrom)
-- }}}

-- Functions {{{

nqueens :: MonadPlus m ⇒ Int → m [Int] -- {{{
nqueens n = go n 0 (IntSet.fromDistinctAscList [0..n-1]) IntSet.empty IntSet.empty []
  where
    go 0 _ _ _ _ !positions = return (reverse positions)
    go !n
       !row
       !available_columns
       !occupied_negative_diagonals
       !occupied_positive_diagonals
       !positions
     = do
        allFrom
            [ (column,negative_diagonal,positive_diagonal)
            | column ← IntSet.toList available_columns
            , let negative_diagonal = row + column
            , IntSet.notMember negative_diagonal occupied_negative_diagonals
            , let positive_diagonal = row - column
            , IntSet.notMember positive_diagonal occupied_positive_diagonals
            ]
        >>=
        \(column,negative_diagonal,positive_diagonal) →
            go (n-1)
               (row+1)
               (IntSet.delete column available_columns)
               (IntSet.insert negative_diagonal occupied_negative_diagonals)
               (IntSet.insert positive_diagonal occupied_positive_diagonals)
               (column:positions)
{-# SPECIALIZE nqueens :: Int → [[Int]] #-}
{-# SPECIALIZE nqueens :: Int → Visitor [Int] #-}
-- }}}

-- }}}