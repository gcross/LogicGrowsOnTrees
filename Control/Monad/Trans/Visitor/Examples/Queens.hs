-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import Data.Bits ((.&.),clearBit,setBit,shiftL,shiftR,testBit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word64)

import Control.Monad.Trans.Visitor (Visitor,allFromGreedy)
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
nqueens n = go n 0 ((1 `shiftL` n)-1::Word64) (0::Word64) (0::Word64) []
  where
    negative_diagonal_offset = n-1
    go 0 _ _ _ _ !positions = return (reverse positions)
    go !numbers_of_rows_remaining
       !row
       !available_columns
       !occupied_negative_diagonals
       !occupied_positive_diagonals
       !positions
     = do
        allFromGreedy
            [ (column,negative_diagonal,positive_diagonal)
            | column ← go1 0 available_columns
            , let negative_diagonal = row + column
                  positive_diagonal = row - column + negative_diagonal_offset
            , not $ (occupied_negative_diagonals `testBit` negative_diagonal)
                 || (occupied_positive_diagonals `testBit` positive_diagonal)
            ]
        >>=
        \(column,negative_diagonal,positive_diagonal) →
            go (numbers_of_rows_remaining-1)
               (row+1)
               (available_columns `clearBit` column)
               (occupied_negative_diagonals `setBit` negative_diagonal)
               (occupied_positive_diagonals `setBit` positive_diagonal)
               (column:positions)
    go1 _ 0 = []
    go1 !i !bits
     | (bits .&. 1 == 1) = i:next
     | otherwise         =   next
     where
       next = go1 (i+1) (bits `shiftR` 1)
{-# SPECIALIZE nqueens :: Int → [[Int]] #-}
{-# SPECIALIZE nqueens :: Int → Visitor [Int] #-}
-- }}}

-- }}}