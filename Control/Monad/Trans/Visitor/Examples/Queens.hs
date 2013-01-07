-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import Data.Bits ((.&.),(.|.),bit,clearBit,rotateL,rotateR,setBit,testBit,unsafeShiftL,unsafeShiftR)
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
    ,   s_window_start :: {-# UNPACK #-} !Int
    ,   s_window_size :: {-# UNPACK #-} !Int
    ,   s_row :: {-# UNPACK #-} !Int
    ,   s_occupied_rows :: {-# UNPACK #-} !Word64
    ,   s_occupied_columns :: {-# UNPACK #-} !Word64
    ,   s_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   s_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }
-- }}}

data PositionAndBit = PositionAndBit {-# UNPACK #-} !Int {-# UNPACK #-} !Word64

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

getOpenings :: MonadPlus m ⇒ Int → Word64 → m PositionAndBit -- {{{
getOpenings size blocked
    | blocked .&. mask == mask = mzero
    | otherwise = allFromGreedy $ go (PositionAndBit 0 1)
  where
    mask = bit size - 1
    go x@(PositionAndBit i b)
     | i >= size            =   []
     | (b .&. blocked == 0) = x:next
     | otherwise            =   next
     where
       next = go $ PositionAndBit (i+1) (b `unsafeShiftL` 1)
{-# INLINE getOpenings #-}
-- }}}

nqueensCorrectCount :: Int → Int -- {{{
nqueensCorrectCount = fromJust . ($ nqueens_correct_counts) . IntMap.lookup
-- }}}

nqueensGeneric :: MonadPlus m ⇒ α → NQueensCallbacks α β → Int → m β -- {{{
nqueensGeneric initial_value NQueensCallbacks{..} 1 = return . finalizeValue . updateValue (0,0) $ initial_value
nqueensGeneric initial_value NQueensCallbacks{..} 2 = mzero
nqueensGeneric initial_value NQueensCallbacks{..} 3 = mzero
nqueensGeneric initial_value callbacks n = nqueensSearch callbacks initial_value $ NQueensSearchState n 0 n 0 0 0 0 0
{-# INLINE nqueensGeneric #-}
-- }}}

nqueensSearch :: MonadPlus m ⇒ NQueensCallbacks α β → α → NQueensSearchState → m β -- {{{
nqueensSearch NQueensCallbacks{..} = go
  where
    go !value
       !(NQueensSearchState
            number_of_queens_remaining
            window_start
            window_size
            row
            occupied_rows
            occupied_columns
            occupied_negative_diagonals
            occupied_positive_diagonals
       )
      | occupied_rows .&. 1 == 0 =
         (getOpenings window_size $
            occupied_columns .|. occupied_negative_diagonals .|. occupied_positive_diagonals
         )
         >>=
         \(PositionAndBit offset offset_bit) →
            if number_of_queens_remaining == 1
                then return $ finalizeValue ((row,window_start+offset) `updateValue` value)
                else go ((row,window_start+offset) `updateValue` value) $
                    if offset == 0 || offset == (window_size-1)
                        then if offset == 0
                                then let !shift = go (PositionAndBit 1 2)
                                           where
                                             go !(PositionAndBit i b)
                                               | b .&. occupied_columns == 0 = i
                                               | otherwise = go $ PositionAndBit (i+1) (b `unsafeShiftL` 1)
                                     in NQueensSearchState
                                        (number_of_queens_remaining-1)
                                        (window_start+shift)
                                        (window_size-shift)
                                        (row+1)
                                        (occupied_rows `unsafeShiftR` 1)
                                        ((occupied_columns .|. offset_bit) `unsafeShiftR` shift)
                                        ((occupied_negative_diagonals .|. offset_bit) `unsafeShiftR` (shift+1))
                                        ((occupied_positive_diagonals .|. offset_bit) `rotateR` (shift-1))
                                else let !new_window_size
                                           | offset == window_size-1 = go $ PositionAndBit (window_size-1) (bit (offset-1))
                                           | otherwise = window_size
                                           where
                                             go !(PositionAndBit i b)
                                               | b .&. occupied_columns == 0 = i
                                               | otherwise = go $ PositionAndBit (i-1) (b `unsafeShiftR` 1)
                                     in NQueensSearchState
                                        (number_of_queens_remaining-1)
                                         window_start
                                         new_window_size
                                        (row+1)
                                        (occupied_rows `unsafeShiftR` 1)
                                        (occupied_columns .|. offset_bit)
                                        ((occupied_negative_diagonals .|. offset_bit) `rotateR` 1)
                                        ((occupied_positive_diagonals .|. offset_bit) `rotateL` 1)
                        else NQueensSearchState
                                        (number_of_queens_remaining-1)
                                         window_start
                                         window_size
                                        (row+1)
                                        (occupied_rows `unsafeShiftR` 1)
                                        (occupied_columns .|. offset_bit)
                                        ((occupied_negative_diagonals .|. offset_bit) `rotateR` 1)
                                        ((occupied_positive_diagonals .|. offset_bit) `rotateL` 1)
      | otherwise =
         go value
            (NQueensSearchState
                 number_of_queens_remaining
                 window_start
                 window_size
                (row+1)
                (occupied_rows `unsafeShiftR` 1)
                 occupied_columns
                (occupied_negative_diagonals `rotateR` 1)
                (occupied_positive_diagonals `rotateL` 1)
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