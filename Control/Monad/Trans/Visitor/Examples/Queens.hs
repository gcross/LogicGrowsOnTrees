-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import Data.Bits ((.&.),(.|.),bit,clearBit,rotateL,rotateR,setBit,testBit,unsafeShiftL)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Word (Word64)

import Control.Monad.Trans.Visitor (Visitor,allFromGreedy)
-- }}}

-- Types {{{

data NQueensCallbacks α β = NQueensCallbacks -- {{{
    {   updateValue :: !((Int,Int) → α → α)
    ,   finalizeValue :: !(α → β)
    }
-- }}}

data NQueensSearchState = NQueensSearchState -- {{{
    {   s_number_of_queens_remaining :: {-# UNPACK #-} !Int
    ,   row :: {-# UNPACK #-} !Int
    ,   row_bit :: {-# UNPACK #-} !Word64
    ,   s_occupied :: {-# UNPACK #-} !NQueensOccupied
    }
-- }}}

data NQueensOccupied = NQueensOccupied -- {{{
    {   occupied_rows :: {-# UNPACK #-} !Word64
    ,   occupied_columns :: {-# UNPACK #-} !Word64
    ,   occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }
-- }}}

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

getOpenings :: Int → Int → Word64 → [(Int,Word64)] -- {{{
getOpenings start end blocked = go start 1
  where
    go !i !b
     | i > end              =       []
     | (b .&. blocked == 0) = (i,b):next
     | otherwise            =       next
     where
       next = go (i+1) (b `unsafeShiftL` 1)
{-# INLINE getOpenings #-}
-- }}}

nqueensCorrectCount :: Int → Int -- {{{
nqueensCorrectCount = fromJust . ($ nqueens_correct_counts) . IntMap.lookup
-- }}}

nqueensGeneric :: MonadPlus m ⇒ α → NQueensCallbacks α β → Int → m β -- {{{
nqueensGeneric initial_value NQueensCallbacks{..} 1 = return . finalizeValue . updateValue (0,0) $ initial_value
nqueensGeneric initial_value NQueensCallbacks{..} 2 = mzero
nqueensGeneric initial_value NQueensCallbacks{..} 3 = mzero
nqueensGeneric initial_value callbacks@NQueensCallbacks{..} n =
    nqueensSearch callbacks initial_value n 0 (n-1) (NQueensOccupied 0 0 0 0)
{-# INLINE nqueensGeneric #-}
-- }}}

nqueensSearch :: MonadPlus m ⇒ NQueensCallbacks α β → α → Int → Int → Int → NQueensOccupied → m β -- {{{
nqueensSearch NQueensCallbacks{..} value number_of_queens_remaining first last occupied =
    go value $ NQueensSearchState number_of_queens_remaining first (bit first) occupied
  where
    go !value !(NQueensSearchState{s_number_of_queens_remaining=0}) = return (finalizeValue value)
    go !value !(NQueensSearchState{s_occupied=NQueensOccupied{..},..})
      | row_bit .&. occupied_rows == 0 =
         (allFromGreedy . getOpenings first last $
            occupied_columns .|. occupied_negative_diagonals .|. occupied_positive_diagonals
         )
         >>=
         \(column,column_bit) → go
            ((row,column) `updateValue` value)
            (NQueensSearchState
                (s_number_of_queens_remaining-1)
                (row+1)
                (row_bit `unsafeShiftL` 1)
                (NQueensOccupied
                    (occupied_rows .|. row_bit)
                    (occupied_columns .|. column_bit)
                    ((occupied_negative_diagonals .|. column_bit) `rotateR` 1)
                    ((occupied_positive_diagonals .|. column_bit) `rotateL` 1)
                )
            )
      | otherwise =
         go value
            (NQueensSearchState
                 s_number_of_queens_remaining
                (row+1)
                (row_bit `unsafeShiftL` 1)
                (NQueensOccupied
                     occupied_rows
                     occupied_columns
                    (occupied_negative_diagonals `rotateR` 1)
                    (occupied_positive_diagonals `rotateL` 1)
                )
            )
{-# INLINE nqueensSearch #-}
-- }}}

nqueensCount :: MonadPlus m ⇒ Int → m (Sum Int) -- {{{
nqueensCount = nqueensGeneric () $ NQueensCallbacks (const id) (const (Sum 1))
{-# SPECIALIZE nqueensCount :: Int → [Sum Int] #-}
{-# SPECIALIZE nqueensCount :: Int → Visitor (Sum Int) #-}
-- }}}

nqueensSolutions :: MonadPlus m ⇒ Int → m [(Int,Int)] -- {{{
nqueensSolutions = nqueensGeneric [] $ NQueensCallbacks (:) sort
{-# SPECIALIZE nqueensSolutions :: Int → [[(Int,Int)]] #-}
{-# SPECIALIZE nqueensSolutions :: Int → Visitor [(Int,Int)] #-}
-- }}}

nqueensTrivial :: MonadPlus m ⇒ Int → m () -- {{{
nqueensTrivial = nqueensGeneric () $ NQueensCallbacks (const id) (const ())
{-# SPECIALIZE nqueensTrivial :: Int → [()] #-}
-- }}}

-- }}}