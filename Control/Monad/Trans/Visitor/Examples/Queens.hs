-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import Data.Bits ((.&.),(.|.),clearBit,rotateL,rotateR,setBit,testBit,unsafeShiftL)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Word (Word64)

import Control.Monad.Trans.Visitor (Visitor,allFromGreedy)
-- }}}

data NQueensState = NQueensState
    {   number_of_rows_remaining :: {-# UNPACK #-} !Int
    ,   row :: {-# UNPACK #-} !Int
    ,   row_bit :: {-# UNPACK #-} !Word64
    ,   occupied_rows :: {-# UNPACK #-} !Word64
    ,   occupied_columns :: {-# UNPACK #-} !Word64
    ,   occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }

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

nqueensCorrectCount :: Int → Int -- {{{
nqueensCorrectCount = fromJust . ($ nqueens_correct_counts) . IntMap.lookup
-- }}}

nqueensGeneric :: MonadPlus m ⇒ α → ((Int,Int) → α → α) → (α → β) → Int → m β -- {{{
nqueensGeneric initial_value updateValue finalizeValue n =
    go initial_value $ NQueensState n 0 1 0 0 0 0
  where
    go !value !(NQueensState{number_of_rows_remaining=0}) = return (finalizeValue value)
    go !value !(NQueensState{..})
      | row_bit .&. occupied_rows == 0 = do
         allFromGreedy columns
         >>=
         \(column,column_bit) → go
            ((row,column) `updateValue` value)
            (NQueensState
                (number_of_rows_remaining-1)
                (row+1)
                (row_bit `unsafeShiftL` 1)
                (occupied_rows .|. row_bit)
                (occupied_columns .|. column_bit)
                ((occupied_negative_diagonals .|. column_bit) `rotateR` 1)
                ((occupied_positive_diagonals .|. column_bit) `rotateL` 1)
            )
      | otherwise =
         go value
            (NQueensState
                 number_of_rows_remaining
                (row+1)
                (row_bit `unsafeShiftL` 1)
                 occupied_rows
                 occupied_columns
                (occupied_negative_diagonals `rotateR` 1)
                (occupied_positive_diagonals `rotateL` 1)
            )
      where
        blocked_columns = occupied_columns .|. occupied_negative_diagonals .|. occupied_positive_diagonals
        columns = goColumns 0 1
          where
            goColumns !i !b
             | i == n                         =       []
             | (b .&. blocked_columns == 0)   = (i,b):next
             | otherwise                      =       next
             where
               next = goColumns (i+1) (b `unsafeShiftL` 1)
{-# INLINE nqueensGeneric #-}
-- }}}

nqueensCount :: MonadPlus m ⇒ Int → m (Sum Int) -- {{{
nqueensCount = nqueensGeneric () (const id) (const (Sum 1))
{-# SPECIALIZE nqueensCount :: Int → [Sum Int] #-}
{-# SPECIALIZE nqueensCount :: Int → Visitor (Sum Int) #-}
-- }}}

nqueensSolutions :: MonadPlus m ⇒ Int → m [(Int,Int)] -- {{{
nqueensSolutions = nqueensGeneric [] (:) sort
{-# SPECIALIZE nqueensSolutions :: Int → [[(Int,Int)]] #-}
{-# SPECIALIZE nqueensSolutions :: Int → Visitor [(Int,Int)] #-}
-- }}}

nqueensTrivial :: MonadPlus m ⇒ Int → m () -- {{{
nqueensTrivial = nqueensGeneric () (const id) (const ())
{-# SPECIALIZE nqueensTrivial :: Int → [()] #-}
-- }}}

-- }}}