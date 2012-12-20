-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Control.Monad.Trans.Visitor (Visitor,allFrom)
-- }}}

-- Values -- {{{

nqueens_correct_counts :: IntMap Int
nqueens_correct_counts = IntMap.fromDistinctAscList
    [( 1,1)
    ,( 2,0)
    ,( 3,0)
    ,( 4,2)
    ,( 5,10)
    ,( 6,4)
    ,( 7,40)
    ,( 8,92)
    ,( 9,352)
    ,(10,724)
    ,(11,2680)
    ,(12,14200)
    ,(13,73712)
    ,(14,365596)
    ,(15,2279184)
    ,(16,14772512)
    ,(17,95815104)
    ,(18,666090624)
    -- Commented out in case Int is not 64-bit
    --,(19,4968057848)
    --,(20,39029188884)
    --,(21,314666222712)
    --,(22,2691008701644)
    --,(23,24233937684440)
    ]
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