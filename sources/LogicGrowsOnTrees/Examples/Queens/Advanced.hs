{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

{-|
This module contains a heavily optimized solver for the n-queens problems.
Specifically, it uses the following tricks:

  * Symmetry breaking to prune redundant solutions

  * Unpacked datatypes instead of multiple arguments

  * Heavily optimized 'getOpenings' that uses bottom-up tree building

  * C code for the inner-most loop

  * INLINEs in many places in order to create optimized specializations of
    the generic functions

Benchmarks were used to determine that all of these tricks resulted in
performance improvements using GHC 7.4.3.
 -}
module LogicGrowsOnTrees.Examples.Queens.Advanced
    (
    -- * Types
      NQueensSymmetry(..)
    , NQueensSolution
    , NQueensSolutions
    , PositionAndBit
    , PositionAndBitWithReflection
    -- * Main algorithm
    , nqueensGeneric
    -- ** Symmetry breaking
    -- $symmetry-breaking
    , nqueensStart
    , NQueensBreak90State(..)
    , nqueensBreak90
    , NQueensBreak180State(..)
    , nqueensBreak180
    -- ** Brute-force search
    -- $brute-force
    , NQueensSearchState(..)
    , nqueensSearch
    , nqueensBruteForceGeneric
    , nqueensBruteForceSolutions
    , nqueensBruteForceCount
    -- ** C inner-loop
    , c_LogicGrowsOnTrees_Queens_count_solutions
    , mkPushValue
    , mkPopValue
    , mkFinalizeValue
    , nqueensCSearch
    , nqueensCGeneric
    , nqueensCSolutions
    , nqueensCCount
    -- * Helper functions
    , allRotationsAndReflectionsOf
    , allRotationsOf
    , convertSolutionToWord
    , extractExteriorFromSolution
    , getOpenings
    , getOpeningsAsList
    , getSymmetricOpenings
    , hasReflectionSymmetry
    , hasRotate90Symmetry
    , hasRotate180Symmetry
    , multiplicityForSymmetry
    , multiplySolution
    , reflectBits
    , reflectSolution
    , rotate180
    , rotateLeft
    , rotateRight
    , symmetryOf
    ) where

import Control.Applicative ((<$>),liftA2)
import Control.Arrow ((***))
import Control.Exception (evaluate)
import Control.Monad (MonadPlus(..),(>=>),liftM,liftM2)

import Data.Bits ((.&.),(.|.),bit,rotateL,rotateR,unsafeShiftL,unsafeShiftR)
import Data.Function (on)
import Data.IORef (modifyIORef,newIORef,readIORef,writeIORef)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable(..),cast)
import Data.Word (Word,Word64)

import Foreign.C.Types (CUInt(..))
import Foreign.Ptr (FunPtr,freeHaskellFunPtr,nullFunPtr)

import System.IO.Unsafe (unsafePerformIO)

import LogicGrowsOnTrees (Tree,between)
import LogicGrowsOnTrees.Utils.MonadPlusForest (addToForest,emptyForest,consolidateForest)
import LogicGrowsOnTrees.Utils.WordSum

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

{-| This datatype is used to represent one of the possible board symmetries. -}
data NQueensSymmetry =
    NoSymmetries {-^ the board has no symmetries at all -}
  | Rotate180Only {-^ the board is symmetric under 180 degree rotations -}
  | AllRotations {-^ the board is symmetric under all rotations -}
  | AllSymmetries {-^ the board is symmetric under all rotations and reflections -}
  deriving (Eq,Ord,Read,Show)

{-| Type alias for a solution, which takes the form of a list of coordinates. -}
type NQueensSolution = [(Word,Word)]

{-| Type alias for a list of solutions. -}
type NQueensSolutions = [NQueensSolution]

{-| Represents a position and bit at that position. -}
data PositionAndBit = PositionAndBit {-# UNPACK #-} !Int {-# UNPACK #-} !Word64

{-| Like 'PositionAndBit', but also including the same for the reflection of the
    position (i.e., one less than the board size minus the position).
 -}
data PositionAndBitWithReflection = PositionAndBitWithReflection {-# UNPACK #-} !Int {-# UNPACK #-} !Word64  {-# UNPACK #-} !Int {-# UNPACK #-} !Word64

--------------------------------------------------------------------------------
-------------------------------- Main algorithm --------------------------------
--------------------------------------------------------------------------------

{-| Interface to the main algorithm;  note that α and β need to be 'Typeable'
    because of an optimization used in the C part of the code.
 -}
nqueensGeneric ::
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (Word → NQueensSymmetry → α → m β) {-^ finalizes a solution with the given size and symmetry -} →
    α {-^ initial solution -} →
    Word {-^ board size -} →
    m β {-^ the final result -}
nqueensGeneric updateValue finalizeValueWithSymmetry initial_value 1 =
    finalizeValueWithSymmetry 1 AllSymmetries . updateValue [(0,0)] $ initial_value
nqueensGeneric _ _ _ 2 = mzero
nqueensGeneric _ _ _ 3 = mzero
nqueensGeneric updateValue finalizeValueWithSymmetry initial_value n =
    nqueensStart
        updateValue
        break90
        break180
        search
        initial_value
        n
  where
    break90 = nqueensBreak90 updateValue (finalizeValueWithSymmetry n AllRotations) break90 break180 search
    break180 = nqueensBreak180 updateValue (finalizeValueWithSymmetry n Rotate180Only) break180 search
    search value size state = nqueensSearch updateValue (finalizeValueWithSymmetry n NoSymmetries) value size state
{-# INLINE nqueensGeneric #-}

--------------------------------------------------------------------------------
------------------------------ Symmetry-breaking -------------------------------
--------------------------------------------------------------------------------

{- $symmetry-breaking
A performance gain can be obtained by factoring out symmetries because if, say,
a solution has rotational symmetry, then that means that there are four
configurations that are equivalent, and so we would ideally like to prune three
of these four equivalent solution from the tree.

I call the approach used here "symmetry breaking". The idea is we start with a
perfectly symmetrical board (as it has nothing on it) and then proceed from the
outside layer in, where by a layer I am referring to the n-th square in starting
from the outside border. At each step we either preserve a given symmetry for
the current layer or we break it; in the former case we stay within the current
routine to try to break it in the next layer in, in the latter case we jump to a
routine designed to break the new symmetry in the next layer in. When all
symmetries have been broken, we jump to the brute-force search code. If we place
all of the queens while having preserved one or more symmetries, then either we
apply the rotations and reflects of the symmetry to generate all of the
solutions or we multiply the solution count by the number of equivalent
solutions.

This code is unforunately quite complicated because there are many possibilities
for how to break or not break the symmetries and at each step it has to place
between 0 and 4 queens in such a way as to not conflict with any queen that has
already been placed.

Also, each function takes callbacks for each symmetry rather than directly
calling 'nqueensBreak90', etc. in order to ease testing.

-}

-------------------------------- All symmeties ---------------------------------

{-| Break the reflection symmetry. -}
nqueensStart ::
    MonadPlus m ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → NQueensBreak90State → m β) {-^ break the rotational symmetry for the next inner layer -} →
    (α → NQueensBreak180State → m β) {-^ break the 180-degree rotational symmetry for the next inner layer -} →
    (α → Int → NQueensSearchState → m β) {-^ brute-force search -} →
    α {-^ partial solution -} →
    Word {-^ board size -} →
    m β {-^ the final result -}
nqueensStart
  !updateValue_
  !break90
  !break180
  !search
  !value
  !n = (preserve90 `mplus` breakTo180) `mplus` (breakAtCorner `mplus` breakAtSides)
  where
    updateValue = updateValue_ . convertSolutionToWord
    half_inner_size = fromIntegral $ (n `div` 2) - 1
    last = fromIntegral $ n-1
    inner_last = last-1

    -- break to 90-degree rotational symmetry
    preserve90 = do
        position ← between 1 half_inner_size
        let reflected_position = last-position
            occupied_bits = bit position .|. bit reflected_position
        break90
            (updateValue
                [(position,last)
                ,(last,reflected_position)
                ,(reflected_position,0)
                ,(0,position)
                ]
                value
            )
            (NQueensBreak90State
                (n-4)
                 1
                (fromIntegral $ n-2)
                (occupied_bits `unsafeShiftR` 1)
                ((occupied_bits .|. (occupied_bits `unsafeShiftL` last)) `unsafeShiftR` 2)
                 (occupied_bits .|. (occupied_bits `rotateR` last))
            )

    -- break to 180-degree rotational symmetry
    breakTo180 = do
        top_column ← between 1 half_inner_size
        right_row ←
            if n .&. 1 == 0
                then between (top_column+1) (last-(top_column+1))
                else between (top_column+1) half_inner_size `mplus`
                     between (half_inner_size+2) (last-(top_column+1))
        let bottom_column = last-top_column
            left_row = last-right_row
            top_column_bit = bit top_column
            right_row_bit = bit right_row
            bottom_column_bit = bit bottom_column
            left_row_bit = bit left_row
        break180
            (updateValue
                [(left_row,last)
                ,(last,bottom_column)
                ,(right_row,0)
                ,(0,top_column)
                ]
                value
            )
            (NQueensBreak180State
                (n-4)
                 1
                (fromIntegral $ n-2)
                ((right_row_bit .|. left_row_bit) `unsafeShiftR` 1)
                ((top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
                ((top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `unsafeShiftL` last)) `unsafeShiftR` 2)
                 (top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `rotateR` last))
                 (right_row_bit .|. top_column_bit)
            )

    -- break all symmetries by placing a queen at a corner
    breakAtCorner = do
        left_row ← between 1 (inner_last-1)
        bottom_column ← between (left_row+1) inner_last
        let left_row_bit = bit left_row
            reflected_left_row_bit = bit (last-left_row)
            bottom_column_bit = bit bottom_column
        search
            (updateValue
                [(last,bottom_column)
                ,(left_row,last)
                ,(0,0)
                ]
                value
            )
            (fromIntegral $ n-2)
            (NQueensSearchState
                (n-3)
                 1
                (left_row_bit `unsafeShiftR` 1)
                (bottom_column_bit `unsafeShiftR` 1)
                ((left_row_bit .|. bottom_column_bit) `unsafeShiftL` (last-2))
                (1 .|. reflected_left_row_bit .|. (bottom_column_bit `rotateR` last))
            )

    -- break all symmetries without placing a queen at a corner
    breakAtSides = do
        top_column ← between 1 half_inner_size
        let reflected_top_column = last-top_column
            after_top_column = top_column+1
            reflected_after_top_column = reflected_top_column-1
        right_row ← between after_top_column reflected_after_top_column
        let reflected_right_row = last-right_row
        bottom_column ←
            between after_top_column (reflected_right_row-1) `mplus`
            between (reflected_right_row+1) reflected_top_column
        left_row ←
            if bottom_column == reflected_top_column
                then if reflected_right_row < right_row
                        then between top_column (reflected_right_row-1)
                        else between top_column (right_row-1) `mplus`
                             between (right_row+1) (reflected_right_row-1)
                else let (first,second)
                           | right_row < bottom_column = (right_row,bottom_column)
                           | otherwise                 = (bottom_column,right_row)
                     in  between top_column (first-1) `mplus`
                         between (first+1) (second-1) `mplus`
                         between (second+1) reflected_after_top_column
        let top_column_bit = bit top_column
            right_row_bit = bit right_row
            bottom_column_bit = bit bottom_column
            left_row_bit = bit left_row
        search
            (updateValue
                [(left_row,last)
                ,(last,bottom_column)
                ,(right_row,0)
                ,(0,top_column)
                ]
                value
            )
            (fromIntegral $ n-2)
            (NQueensSearchState
                (n-4)
                 1
                ((left_row_bit .|. right_row_bit) `unsafeShiftR` 1)
                ((top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
                ((top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `unsafeShiftL` last)) `unsafeShiftR` 2)
                 (top_column_bit .|. bit (last-left_row) .|. (1 `rotateR` right_row) .|. (bottom_column_bit `rotateR` last))
            )

------------------------ 90-degree rotational symmetry -------------------------

{-| The state type while the 90-degree rotational symmetry is being broken. -}
data NQueensBreak90State = NQueensBreak90State
    {   b90_number_of_queens_remaining :: {-# UNPACK #-} !Word
    ,   b90_window_start :: {-# UNPACK #-} !Int
    ,   b90_window_size :: {-# UNPACK #-} !Int
    ,   b90_occupied_rows_and_columns :: {-# UNPACK #-} !Word64
    ,   b90_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   b90_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }

{-| Break the 90-degree rotational symmetry at the current layer. -}
nqueensBreak90 ::
    MonadPlus m ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → m β) {-^ finalize the solution -} →
    (α → NQueensBreak90State → m β) {-^ break the rotational symmetry for the next inner layer -} →
    (α → NQueensBreak180State → m β) {-^ break the 180-degree rotational symmetry for the next inner layer -} →
    (α → Int → NQueensSearchState → m β) {-^ brute-force search -} →
    α {-^ partial solution -} →
    NQueensBreak90State {-^ current state -} →
    m β {-^ the final result -}
nqueensBreak90
  !updateValue_
  !finalizeValue
  !break90
  !break180
  !search
  !value
  !(NQueensBreak90State
      number_of_queens_remaining
      window_start
      window_size
      occupied_rows_and_columns
      occupied_negative_diagonals
      occupied_positive_diagonals
  )
  | number_of_queens_remaining == 0 = finalizeValue value
  | window_size > 3 =
     if occupied_rows_and_columns .&. 1 == 0
        then keep90 `mplus` breakTo180 `mplus`
                if occupied_negative_diagonals .&. 1 == 0
                    then breakAtCorner `mplus` breakAtSides
                    else breakAtSides
        else nextWindow
  | number_of_queens_remaining == 1 && occupied_rows_and_columns .&. 2 == 0 =
     finalizeValue ([(window_start+1,window_start+1)] `updateValue` value)
  | otherwise = mzero
  where
    updateValue = updateValue_ . convertSolutionToWord
    window_end = window_start+window_size-1
    end = window_size-1
    inner_size = window_size-2
    inner_end = window_size-3
    blocked = occupied_rows_and_columns .|. occupied_negative_diagonals .|. occupied_positive_diagonals
    inner_blocked = blocked `unsafeShiftR` 1
    inner_blocked_excluding_middle
      | window_size .&. 1 == 0 = inner_blocked
      | otherwise = inner_blocked .|. bit (inner_size `div` 2)

    -- place queens to preserve all rotational symmetries
    keep90 = do
        PositionAndBitWithReflection offset offset_bit reflected_offset reflected_offset_bit ←
            getSymmetricOpenings inner_size inner_blocked_excluding_middle
        let position = window_start+offset+1
            reflected_position = window_start+reflected_offset+1
            occupied_bits = (offset_bit .|. reflected_offset_bit) `unsafeShiftL` 1
        break90
            (updateValue
                [(position,window_end)
                ,(window_end,reflected_position)
                ,(reflected_position,window_start)
                ,(window_start,position)
                ]
                value
            )
            (NQueensBreak90State
                (number_of_queens_remaining-4)
                (window_start+1)
                (window_size-2)
                ((occupied_rows_and_columns .|. occupied_bits) `unsafeShiftR` 1)
                ((occupied_negative_diagonals .|. occupied_bits .|. (occupied_bits `unsafeShiftL` end)) `unsafeShiftR` 2)
                 (occupied_positive_diagonals .|. occupied_bits .|. (occupied_bits `rotateR` end))
            )

    -- place queens to break down to 180-degree rotational symmetry
    breakTo180 = do
        PositionAndBit inner_top_column inner_top_column_bit ←
            getOpenings (inner_size-1) inner_blocked_excluding_middle
        PositionAndBit inner_right_row _ ←
            getOpenings (inner_end-inner_top_column) (inner_blocked_excluding_middle .|. inner_top_column_bit)
        let top_column = inner_top_column+1
            bottom_column = end-top_column
            right_row = inner_right_row+1
            left_row = end-right_row
            top_column_bit = bit top_column
            bottom_column_bit = bit bottom_column
            right_row_bit = bit right_row
            left_row_bit = bit left_row
            new_occupied_positive_diagonals = occupied_positive_diagonals .|. top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `rotateR` end)
        break180
            (updateValue
               [(window_start+left_row,window_end)
               ,(window_end,window_start+bottom_column)
               ,(window_start+right_row,window_start)
               ,(window_start,window_start+top_column)
               ]
               value
            )
            (NQueensBreak180State
                (number_of_queens_remaining-4)
                (window_start+1)
                (window_size-2)
                ((occupied_rows_and_columns .|. right_row_bit .|. left_row_bit) `unsafeShiftR` 1)
                ((occupied_rows_and_columns .|. top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
                ((occupied_negative_diagonals .|. top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `unsafeShiftL` end)) `unsafeShiftR` 2)
                 new_occupied_positive_diagonals
                (reflectBits new_occupied_positive_diagonals)
            )

    -- fully break all symmetries by placing a queen at a corner
    breakAtCorner = do
        PositionAndBit inner_left_row inner_left_row_bit ←
            getOpenings inner_size inner_blocked
        PositionAndBit inner_bottom_column _ ←
            getOpenings inner_size (inner_blocked .|. inner_left_row_bit)
        let left_row = inner_left_row+1
            bottom_column = inner_bottom_column+1                
            left_row_bit = bit left_row
            reflected_left_row_bit = bit (end-left_row)
            bottom_column_bit = bit bottom_column
        search
            (updateValue
                [(window_end,window_start+bottom_column)
                ,(window_start+left_row,window_end)
                ,(window_start,window_start)
                ]
                value
            )
            (window_size-2)
            (NQueensSearchState
                (number_of_queens_remaining-3)
                (window_start+1)
                ((occupied_rows_and_columns .|. left_row_bit) `unsafeShiftR` 1)
                ((occupied_rows_and_columns .|. bottom_column_bit) `unsafeShiftR` 1)
                ((occupied_negative_diagonals .|. ((left_row_bit .|. bottom_column_bit) `unsafeShiftL` end)) `unsafeShiftR` 2)
                 (occupied_positive_diagonals .|. 1 .|. reflected_left_row_bit .|. (bottom_column_bit `rotateR` end))
            )

    -- fully break all symmetries placing no queens at a corner
    breakAtSides = do
        PositionAndBit inner_top_column _ ←
            getOpenings (inner_size-1) inner_blocked
        let inner_reflected_top_column = inner_end-inner_top_column
            inner_reflected_top_column_bit = bit inner_reflected_top_column
            size_of_space_above_inner_top_column = inner_end-inner_top_column
            size_of_space_above_and_including_inner_top_column = size_of_space_above_inner_top_column + 1
            shift_to_inner_top_column = inner_top_column
            shift_to_just_past_inner_top_column = inner_top_column+1
        PositionAndBit inner_right_offset _ ←
            getOpenings
                size_of_space_above_inner_top_column
                ((inner_blocked .|. bit inner_reflected_top_column) `unsafeShiftR` shift_to_just_past_inner_top_column)
        let inner_reflected_right_row = inner_right_offset + inner_top_column + 1
            inner_right_row = inner_end - inner_reflected_right_row 
        PositionAndBit inner_bottom_offset _ ←
            getOpenings
                size_of_space_above_and_including_inner_top_column
                ((inner_blocked .|. inner_reflected_top_column_bit .|. bit inner_right_row) `unsafeShiftR` shift_to_inner_top_column)
        let inner_bottom_column = inner_end - (inner_bottom_offset + inner_top_column)
        PositionAndBit inner_left_row_offset _ ←
            getOpenings
                (if inner_bottom_offset > 0
                    then size_of_space_above_and_including_inner_top_column
                    else inner_reflected_right_row-inner_top_column
                )
                ((inner_blocked .|. bit inner_right_row .|. bit inner_bottom_column .|. inner_reflected_top_column_bit) `unsafeShiftR` shift_to_inner_top_column)
        let top_column = inner_top_column + 1
            right_row = inner_right_row + 1
            bottom_column = inner_bottom_column + 1
            left_row = inner_left_row_offset + inner_top_column + 1
            top_column_bit = bit top_column
            right_row_bit = bit right_row
            bottom_column_bit = bit bottom_column
            left_row_bit = bit left_row
        search
            (updateValue
                [(window_start+left_row,window_end)
                ,(window_end,window_start+bottom_column)
                ,(window_start+right_row,window_start)
                ,(window_start,window_start+top_column)
                ]
                value
            )
            (window_size-2)
            (NQueensSearchState
                (number_of_queens_remaining-4)
                (window_start+1)
                ((occupied_rows_and_columns .|. left_row_bit .|. right_row_bit) `unsafeShiftR` 1)
                ((occupied_rows_and_columns .|. top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
                ((occupied_negative_diagonals .|. top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `unsafeShiftL` end)) `unsafeShiftR` 2)
                 (occupied_positive_diagonals .|. top_column_bit .|. bit (end-left_row) .|. (1 `rotateR` right_row) .|. (bottom_column_bit `rotateR` end))
            )

    -- all squares in this layer are occupied, go to the next one
    nextWindow = break90 value $
        NQueensBreak90State
             number_of_queens_remaining
            (window_start+1)
            (window_size-2)
            (occupied_rows_and_columns `unsafeShiftR` 1)
            (occupied_negative_diagonals `unsafeShiftR` 2)
             occupied_positive_diagonals

----------------------- 180-degree rotational symmetry -------------------------

{-| The state while the 180-degree rotational symmetry is being broken. -}
data NQueensBreak180State = NQueensBreak180State
    {   b180_number_of_queens_remaining :: {-# UNPACK #-} !Word
    ,   b180_window_start :: {-# UNPACK #-} !Int
    ,   b180_window_size :: {-# UNPACK #-} !Int
    ,   b180_occupied_rows :: {-# UNPACK #-} !Word64
    ,   b180_occupied_columns :: {-# UNPACK #-} !Word64
    ,   b180_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   b180_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    ,   b180_occupied_right_positive_diagonals :: {-# UNPACK #-} !Word64
    }

{-| Break the 180-degree rotational symmetry at the current layer. -}
nqueensBreak180 ::
    MonadPlus m ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → m β) {-^ finalize the solution -} →
    (α → NQueensBreak180State → m β) {-^ break the 180-degree rotational symmetry for the next inner layer -} →
    (α → Int → NQueensSearchState → m β) {-^ brute-force search -} →
    α {-^ partial solution -} →
    NQueensBreak180State {-^ current state -} →
    m β {-^ the final result -}
nqueensBreak180
  !updateValue_
  !finalizeValue
  !break180
  !search
  !value
  !(NQueensBreak180State
      number_of_queens_remaining
      window_start
      window_size
      occupied_rows
      occupied_columns
      occupied_negative_diagonals
      occupied_positive_diagonals
      occupied_right_positive_diagonals
  )
  | number_of_queens_remaining == 0 = finalizeValue value
  | window_size > 3 =
     if occupied_rows .&. 1 == 0
        then if occupied_columns .&. 1 == 0
                then mplus preserve180 $
                    if occupied_negative_diagonals .&. end_bit .|. occupied_positive_diagonals .&. end_bit == 0
                        then if occupied_negative_diagonals .&. bit (2*end) .|. occupied_positive_diagonals .&. 1 == 0
                                then breakAtBottomLeftCorner `mplus` breakAtTopLeftCorner `mplus` breakAtSides
                                else breakAtTopLeftCorner `mplus` breakAtSides
                        else if occupied_negative_diagonals .&. bit (2*end) .|. occupied_positive_diagonals .&. 1 == 0
                                then breakAtBottomLeftCorner `mplus` breakAtSides
                                else breakAtSides
                else preserve180Horizontal `mplus` breakAtHorizontalSides
        else if occupied_columns .&. 1 == 0
                then preserve180Vertical `mplus` breakAtVerticalSides
                else nextWindow
  | number_of_queens_remaining == 1 && (occupied_rows .|. occupied_columns) .&. 2 == 0 =
     finalizeValue ([(window_start+1,window_start+1)] `updateValue` value)
  | otherwise = mzero
  where
    updateValue = updateValue_ . convertSolutionToWord
    end = window_size-1
    end_bit = bit end
    window_end = window_start+end
    inner_size = window_size-2
    inner_end = window_size-3
    horizontal_blocked = occupied_columns .|. occupied_negative_diagonals .|. occupied_positive_diagonals
    inner_horizontal_blocked = horizontal_blocked `unsafeShiftR` 1
    inner_horizontal_blocked_excluding_middle
      | window_size .&. 1 == 0 = inner_horizontal_blocked
      | otherwise = inner_horizontal_blocked .|. bit (window_size `div` 2 - 1)
    vertical_blocked = occupied_rows .|. occupied_negative_diagonals .|. occupied_right_positive_diagonals
    inner_vertical_blocked = vertical_blocked `unsafeShiftR` 1
    inner_vertical_blocked_excluding_middle
      | window_size .&. 1 == 0 = inner_vertical_blocked
      | otherwise = inner_vertical_blocked .|. bit (window_size `div` 2 - 1)

    -- break the symmetry without placing a queen at a corner
    breakAtSides = do
        PositionAndBit inner_top_column inner_top_column_bit ←
            getOpenings
                inner_size
                inner_horizontal_blocked
        let inner_reflected_top_column_bit = bit (inner_end - inner_top_column)
        PositionAndBit inner_right_row inner_right_row_bit ←
            getOpenings
                inner_size
               (inner_vertical_blocked .|. inner_top_column_bit)
        PositionAndBit inner_reflected_bottom_column_offset _ ←
            getOpenings
               (inner_end-inner_top_column+1)
              ((inner_horizontal_blocked .|. inner_reflected_top_column_bit .|. inner_right_row_bit) `unsafeShiftR` inner_top_column)
        let inner_reflected_bottom_column = inner_top_column + inner_reflected_bottom_column_offset
            inner_reflected_bottom_column_bit = bit inner_reflected_bottom_column
        PositionAndBit inner_reflected_left_row _ ←
            getOpenings
                (if inner_reflected_bottom_column_offset > 0
                    then inner_size
                    else inner_right_row
                )
                (inner_vertical_blocked .|. bit (inner_end - inner_right_row) .|. inner_reflected_bottom_column_bit .|. inner_top_column_bit)
        let top_column = inner_top_column + 1
            top_column_bit = bit top_column
            right_row = inner_right_row + 1
            right_row_bit = bit right_row
            reflected_right_row_bit = bit (end-right_row)
            bottom_column = inner_end - inner_reflected_bottom_column + 1
            bottom_column_bit = bit bottom_column
            left_row = inner_end - inner_reflected_left_row + 1
            left_row_bit = bit left_row
            reflected_left_row_bit = bit (end-left_row)
        search
            (updateValue
                [(window_start+left_row,window_end)
                ,(window_end,window_start+bottom_column)
                ,(window_start+right_row,window_start)
                ,(window_start,window_start+top_column)
                ]
                value
            )
            (window_size-2)
            (NQueensSearchState
                (number_of_queens_remaining-4)
                (window_start+1)
               ((occupied_rows .|. right_row_bit .|. left_row_bit) `unsafeShiftR` 1)
               ((occupied_columns .|. top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
               ((occupied_negative_diagonals .|. top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `unsafeShiftL` end)) `unsafeShiftR` 2)
                (occupied_positive_diagonals .|. top_column_bit .|. reflected_left_row_bit .|. ((bottom_column_bit .|. reflected_right_row_bit) `rotateR` end))
            )

    -- break the symmetry by placing queens only on the horizontal sides
    breakAtHorizontalSides = do
        PositionAndBit inner_top_column _ ←
            getOpenings
               (inner_size-1)
                inner_horizontal_blocked
        PositionAndBit inner_reflected_bottom_column_offset _ ←
            getOpenings
               (inner_end-inner_top_column)
              ((inner_horizontal_blocked .|. bit (inner_end - inner_top_column)) `unsafeShiftR` (inner_top_column+1))
        let top_column = inner_top_column + 1
            top_column_bit = bit top_column
            bottom_column = inner_end - (inner_top_column + inner_reflected_bottom_column_offset + 1) + 1
            bottom_column_bit = bit bottom_column
        search
            (updateValue
                [(window_end,window_start+bottom_column)
                ,(window_start,window_start+top_column)
                ]
                value
           )
           (window_size-2)
           (NQueensSearchState
                (number_of_queens_remaining-2)
                (window_start+1)
                (occupied_rows `unsafeShiftR` 1)
               ((occupied_columns .|. top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
               ((occupied_negative_diagonals .|. top_column_bit .|. (bottom_column_bit `unsafeShiftL` end)) `unsafeShiftR` 2)
                (occupied_positive_diagonals .|. top_column_bit .|. (bottom_column_bit `rotateR` end))
           )

    -- break the symmetry by placing queens only on the vertical sides
    breakAtVerticalSides = do
        PositionAndBit inner_right_row _ ←
            getOpenings
               (inner_size-1)
                inner_vertical_blocked
        PositionAndBit inner_reflected_left_row_offset _ ←
            getOpenings
               (inner_end-inner_right_row)
              ((inner_vertical_blocked .|. bit (inner_end - inner_right_row)) `unsafeShiftR` (inner_right_row+1))
        let right_row = inner_right_row + 1
            right_row_bit = bit right_row
            reflected_right_row_bit = bit (end-right_row)
            left_row = inner_end - (inner_right_row + inner_reflected_left_row_offset + 1) + 1
            left_row_bit = bit left_row
            reflected_left_row_bit = bit (end-left_row)
        search
            (updateValue
                [(window_start+left_row,window_end)
                ,(window_start+right_row,window_start)
                ]
                value
            )
            (window_size-2)
            (NQueensSearchState
                (number_of_queens_remaining-2)
                (window_start+1)
               ((occupied_rows .|. right_row_bit .|. left_row_bit) `unsafeShiftR` 1)
                (occupied_columns `unsafeShiftR` 1)
               ((occupied_negative_diagonals .|. right_row_bit .|. (left_row_bit `unsafeShiftL` end)) `unsafeShiftR` 2)
                (occupied_positive_diagonals .|. reflected_left_row_bit .|. (reflected_right_row_bit `rotateR` end))
            )

    -- break by placing a queen at the bottom-left corner
    breakAtBottomLeftCorner = do
        PositionAndBit inner_right_row inner_right_row_bit ←
            getOpenings inner_size inner_vertical_blocked
        PositionAndBit inner_top_column _ ←
            getOpenings inner_size (inner_horizontal_blocked .|. inner_right_row_bit)
        let right_row = inner_right_row+1
            top_column = inner_top_column+1                
            right_row_bit = bit right_row
            reflected_right_row_bit = bit (end-right_row)
            top_column_bit = bit top_column
        search
            (updateValue
                [(window_start,window_start+top_column)
                ,(window_start+right_row,window_start)
                ,(window_end,window_end)
                ]
                value
            )
            (window_size-2)
            (NQueensSearchState
                (number_of_queens_remaining-3)
                (window_start+1)
                ((occupied_rows .|. right_row_bit) `unsafeShiftR` 1)
                ((occupied_columns .|. top_column_bit) `unsafeShiftR` 1)
                ((occupied_negative_diagonals .|. right_row_bit .|. top_column_bit) `unsafeShiftR` 2)
                 (occupied_positive_diagonals .|. 1 .|. top_column_bit .|. (reflected_right_row_bit `rotateR` end))
            )

    -- break by placing a queen at the top-left corner
    breakAtTopLeftCorner = do
        PositionAndBit inner_right_row inner_right_row_bit ←
            getOpenings inner_size inner_vertical_blocked
        PositionAndBit inner_reflected_bottom_column _ ←
            getOpenings inner_size (inner_horizontal_blocked .|. inner_right_row_bit)
        let right_row = inner_right_row + 1
            bottom_column = inner_end - inner_reflected_bottom_column + 1                
            right_row_bit = bit right_row
            reflected_right_row_bit = bit (end-right_row)
            bottom_column_bit = bit bottom_column
        search
            (updateValue
                [(window_end,window_start+bottom_column)
                ,(window_start+right_row,window_start)
                ,(window_start,window_end)
                ]
                value
            )
            (window_size-2)
            (NQueensSearchState
                (number_of_queens_remaining-3)
                (window_start+1)
                ((occupied_rows .|. right_row_bit) `unsafeShiftR` 1)
                ((occupied_columns .|. bottom_column_bit) `unsafeShiftR` 1)
                ((occupied_negative_diagonals .|. end_bit .|. right_row_bit .|. (bottom_column_bit `unsafeShiftL` end)) `unsafeShiftR` 2)
                 (occupied_positive_diagonals .|. ((reflected_right_row_bit .|. bottom_column_bit) `rotateR` end))
            )

    -- preserve the 180-degree rotational symmetry
    preserve180 = do
        PositionAndBit inner_top_column inner_top_column_bit ←
            getOpenings
                inner_size
                inner_horizontal_blocked_excluding_middle
        let top_column = inner_top_column + 1
            top_column_bit = bit top_column
            bottom_column = inner_end - inner_top_column + 1
            bottom_column_bit = bit bottom_column
        PositionAndBit inner_right_row _ ←
            getOpenings
                inner_size
               (inner_vertical_blocked_excluding_middle .|. inner_top_column_bit)
        let right_row = inner_right_row + 1
            right_row_bit = bit right_row
            left_row = inner_end - inner_right_row + 1
            left_row_bit = bit left_row
        break180
            (updateValue
                [(window_start+left_row,window_end)
                ,(window_start+right_row,window_start)
                ,(window_end,window_start+bottom_column)
                ,(window_start,window_start+top_column)
                ]
                value
            )
            (NQueensBreak180State
                (number_of_queens_remaining-4)
                (window_start+1)
                (window_size-2)
               ((occupied_rows .|. right_row_bit .|. left_row_bit) `unsafeShiftR` 1)
               ((occupied_columns .|. top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
               ((occupied_negative_diagonals .|. top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `unsafeShiftL` end)) `unsafeShiftR` 2)
                (occupied_positive_diagonals .|. top_column_bit .|. right_row_bit .|. ((bottom_column_bit .|. left_row_bit) `rotateR` end))
                (occupied_right_positive_diagonals .|. top_column_bit .|. right_row_bit)
            )

    -- preserve the 180-degree symmetry for the horizontal sides
    preserve180Horizontal = do
        PositionAndBit inner_top_column _ ←
            getOpenings
                inner_size
                inner_horizontal_blocked_excluding_middle
        let top_column = inner_top_column + 1
            top_column_bit = bit top_column
            bottom_column = inner_end - inner_top_column + 1
            bottom_column_bit = bit bottom_column
        break180
            (updateValue
                [(window_end,window_start+bottom_column)
                ,(window_start,window_start+top_column)
                ]
                value
           )
           (NQueensBreak180State
                (number_of_queens_remaining-2)
                (window_start+1)
                (window_size-2)
                (occupied_rows `unsafeShiftR` 1)
               ((occupied_columns .|. top_column_bit .|. bottom_column_bit) `unsafeShiftR` 1)
               ((occupied_negative_diagonals .|. top_column_bit .|. (bottom_column_bit `unsafeShiftL` end)) `unsafeShiftR` 2)
                (occupied_positive_diagonals .|. top_column_bit .|. (bottom_column_bit `rotateR` end))
                (occupied_right_positive_diagonals .|. top_column_bit)
           )

    -- preserve the 180-degree symmetry for the vertical sides
    preserve180Vertical = do
        PositionAndBit inner_right_row _ ←
            getOpenings
                inner_size
                inner_vertical_blocked_excluding_middle
        let right_row = inner_right_row + 1
            right_row_bit = bit right_row
            left_row = inner_end - inner_right_row + 1
            left_row_bit = bit left_row
        break180
            (updateValue
                [(window_start+left_row,window_end)
                ,(window_start+right_row,window_start)
                ]
                value
            )
            (NQueensBreak180State
                (number_of_queens_remaining-2)
                (window_start+1)
                (window_size-2)
               ((occupied_rows .|. right_row_bit .|. left_row_bit) `unsafeShiftR` 1)
                (occupied_columns `unsafeShiftR` 1)
               ((occupied_negative_diagonals .|. right_row_bit .|. (left_row_bit `unsafeShiftL` end)) `unsafeShiftR` 2)
                (occupied_positive_diagonals .|. right_row_bit .|. (left_row_bit `rotateR` end))
                (occupied_right_positive_diagonals .|. right_row_bit)
            )

    -- all sides are occupied, so go to the next layer in
    nextWindow = break180 value $
        NQueensBreak180State
             number_of_queens_remaining
            (window_start+1)
            (window_size-2)
            (occupied_rows `unsafeShiftR` 1)
            (occupied_columns `unsafeShiftR` 1)
            (occupied_negative_diagonals `unsafeShiftR` 2)
             occupied_positive_diagonals
             occupied_right_positive_diagonals

--------------------------------------------------------------------------------
---------------------------- Brute-force searching -----------------------------
--------------------------------------------------------------------------------

{- $brute-force
After the symmetry has been fully broken, the "brute-force" approach attempts to
place queens in the remaining inner sub-board.  When the number of queens falls
to 10 or less, it farms the rest of the search out to a routine written in C.
 -}

{-| The state during the brute-force search. -}
data NQueensSearchState = NQueensSearchState
    {   s_number_of_queens_remaining :: {-# UNPACK #-} !Word
    ,   s_row :: {-# UNPACK #-} !Int
    ,   s_occupied_rows :: {-# UNPACK #-} !Word64
    ,   s_occupied_columns :: {-# UNPACK #-} !Word64
    ,   s_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   s_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }

{-| Using brute-force to find placements for all of the remaining queens. -}
nqueensSearch ::
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → m β) {-^ finalize the solution -} →
    α {-^ partial solution -} →
    Int {-^ board size -} →
    NQueensSearchState {-^ current state -} →
    m β {-^ the final result -}
nqueensSearch updateValue_ finalizeValue initial_value size initial_search_state@(NQueensSearchState _ window_start _ _ _ _) =
    go initial_value initial_search_state
  where
    updateValue = updateValue_ . convertSolutionToWord
    go !value !s@(NQueensSearchState
                    number_of_queens_remaining
                    row
                    occupied_rows
                    occupied_columns
                    occupied_negative_diagonals
                    occupied_positive_diagonals
               )
      | number_of_queens_remaining <= 10 =
         nqueensCSearch
            updateValue_
            finalizeValue
            value
            size
            window_start
            s
      | occupied_rows .&. 1 == 0 =
         (getOpenings size $
            occupied_columns .|. occupied_negative_diagonals .|. occupied_positive_diagonals
         )
         >>=
         \(PositionAndBit offset offset_bit) → go
            ([(row,window_start+offset)] `updateValue` value)
            (NQueensSearchState
                (number_of_queens_remaining-1)
                (row+1)
                (occupied_rows `unsafeShiftR` 1)
                (occupied_columns .|. offset_bit)
                ((occupied_negative_diagonals .|. offset_bit) `unsafeShiftR` 1)
                ((occupied_positive_diagonals .|. offset_bit) `rotateL` 1)
            )
      | otherwise =
         go value
            (NQueensSearchState
                 number_of_queens_remaining
                (row+1)
                (occupied_rows `unsafeShiftR` 1)
                 occupied_columns
                (occupied_negative_diagonals `unsafeShiftR` 1)
                (occupied_positive_diagonals `rotateL` 1)
            )
{-# INLINE nqueensSearch #-}

{-| Interface for directly using the brute-force search approach -} 
nqueensBruteForceGeneric ::
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → m β) {-^ finalize the solution -} →
    α {-^ initial solution -} →
    Word {-^ board size -} →
    m β {-^ the final result -}
nqueensBruteForceGeneric updateValue finalizeValue initial_value 1 = finalizeValue . updateValue [(0,0)] $ initial_value
nqueensBruteForceGeneric _ _ _ 2 = mzero
nqueensBruteForceGeneric _ _ _ 3 = mzero
nqueensBruteForceGeneric updateValue finalizeValue initial_value n = nqueensSearch updateValue finalizeValue initial_value (fromIntegral n) $ NQueensSearchState n 0 0 0 0 0
{-# INLINE nqueensBruteForceGeneric #-}

{-| Generates the solutions to the n-queens problem with the given board size. -}
nqueensBruteForceSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensBruteForceSolutions = nqueensBruteForceGeneric (++) return []
{-# SPECIALIZE nqueensBruteForceSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensBruteForceSolutions :: Word → Tree NQueensSolution #-}

{-| Generates the solution count to the n-queens problem with the given board size. -}
nqueensBruteForceCount :: MonadPlus m ⇒ Word → m WordSum
nqueensBruteForceCount = nqueensBruteForceGeneric (const id) (const . return $ WordSum 1) ()
{-# SPECIALIZE nqueensBruteForceCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensBruteForceCount :: Word → Tree WordSum #-}

--------------------------------------------------------------------------------
--------------------------------- C inner-loop ---------------------------------
--------------------------------------------------------------------------------

{-| C code that performs a brute-force search for the remaining queens.  The
    last three arguments are allowed to be NULL, in which case they are ignored
    and only the count is returned.
 -}
foreign import ccall safe "queens.h LogicGrowsOnTrees_Queens_count_solutions" c_LogicGrowsOnTrees_Queens_count_solutions ::
    CUInt {-^ board size -} →
    CUInt {-^ number of queens remaining -} →
    CUInt {-^ row number -} →
    Word64 {-^ occupied rows -} →
    Word64 {-^ occupied columns -} →
    Word64 {-^ occupied negative diagonals -} →
    Word64 {-^ occupied positive diagonals -} →
    FunPtr (CUInt -> CUInt → IO ()) {-^ function to push a coordinate on the partial solution;  may be NULL ^-} →
    FunPtr (IO ()) {-^ function to pop a coordinate from partial solution;  may be NULL ^-}  →
    FunPtr (IO ()) {-^ function to finalize a solution;  may be NULL ^-}  →
    IO CUInt

{-| wrapper stub for the push value function pointer -}
foreign import ccall "wrapper" mkPushValue :: (CUInt → CUInt → IO ()) → IO (FunPtr (CUInt → CUInt → IO ()))

{-| wrapper stub for the pop value function pointer -}
foreign import ccall "wrapper" mkPopValue :: IO () → IO (FunPtr (IO ()))

{-| wrapper stub for the finalize value function pointer -}
foreign import ccall "wrapper" mkFinalizeValue :: IO () → IO (FunPtr (IO ()))

{-| Calls C code to perform a brute-force search for the remaining queens.  The
    types α and β must be 'Typeable' because this function actually optimizes
    for the case where only counting is being done by providing null values for
    the function pointer inputs.
 -}
nqueensCSearch ::
    ∀ α m β.
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → m β) {-^ finalize the solution -} →
    α {-^ partial solution -} →
    Int {-^ board size -} →
    Int {-^ window start -} →
    NQueensSearchState {-^ current state -} →
    m β {-^ the final result -}
nqueensCSearch _ finalizeValue value _ _ NQueensSearchState{s_number_of_queens_remaining=0} = finalizeValue value
nqueensCSearch updateValue finalizeValue value size window_start NQueensSearchState{..}
  | typeOf value == typeOf () && typeOf (undefined :: β) == typeOf (undefined :: WordSum) = do
        Just (WordSum multiplier) ← liftM cast (finalizeValue value)
        let number_found =
                fromIntegral
                .
                unsafePerformIO
                $
                c_LogicGrowsOnTrees_Queens_count_solutions
                    (fromIntegral size)
                    (fromIntegral s_number_of_queens_remaining)
                    (fromIntegral s_row)
                    s_occupied_rows
                    s_occupied_columns
                    s_occupied_negative_diagonals
                    s_occupied_positive_diagonals
                    nullFunPtr
                    nullFunPtr
                    nullFunPtr
        return . fromJust . cast $ WordSum (multiplier * number_found)
  | otherwise = unsafePerformIO $ do
        value_stack_ref ← newIORef [value]
        finalized_values ← newIORef mzero
        push_value_funptr ← mkPushValue $ \row offset → modifyIORef value_stack_ref (\stack@(value:_) → updateValue [(fromIntegral row, fromIntegral window_start + fromIntegral offset)] value:stack)
        pop_value_funptr ← mkPopValue $ modifyIORef value_stack_ref tail
        finalize_value_funptr ← mkFinalizeValue $ do
            value ← head <$> readIORef value_stack_ref
            let finalized_value = finalizeValue value
            old_value ← readIORef finalized_values
            new_value ← evaluate $ old_value `mplus` finalized_value
            writeIORef finalized_values new_value
        _ ← c_LogicGrowsOnTrees_Queens_count_solutions
                (fromIntegral size)
                (fromIntegral s_number_of_queens_remaining)
                (fromIntegral s_row)
                s_occupied_rows
                s_occupied_columns
                s_occupied_negative_diagonals
                s_occupied_positive_diagonals
                push_value_funptr
                pop_value_funptr
                finalize_value_funptr
        freeHaskellFunPtr push_value_funptr
        freeHaskellFunPtr pop_value_funptr
        freeHaskellFunPtr finalize_value_funptr
        readIORef finalized_values

{-| Interface for directly using the C search approach -} 
nqueensCGeneric ::
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) {-^ add a list of coordinates to the partial solutions -} →
    (α → m β) {-^ finalize the solution -} →
    α {-^ initial value -} →
    Word {-^ the board size -} →
    m β {-^ the final result -}
nqueensCGeneric updateValue finalizeValue initial_value 1 =
    finalizeValue . updateValue [(0,0)] $ initial_value
nqueensCGeneric _ _ _ 2 = mzero
nqueensCGeneric _ _ _ 3 = mzero
nqueensCGeneric updateValue finalizeValue initial_value n =
    nqueensCSearch updateValue finalizeValue initial_value (fromIntegral n) 0 $
        NQueensSearchState n 0 0 0 0 0
{-# INLINE nqueensCGeneric #-}

{-| Generates the solutions to the n-queens problem with the given board size. -}
nqueensCSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensCSolutions = nqueensCGeneric (++) return []
{-# SPECIALIZE nqueensCSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensCSolutions :: Word → Tree NQueensSolution #-}

{-| Generates the solution count to the n-queens problem with the given board size. -}
nqueensCCount :: MonadPlus m ⇒ Word → m WordSum
nqueensCCount = nqueensCGeneric (const id) (const . return $ WordSum 1) ()
{-# SPECIALIZE nqueensCCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensCCount :: Word → Tree WordSum #-}

--------------------------------------------------------------------------------
------------------------------ Utility functions -------------------------------
--------------------------------------------------------------------------------

{-| Computes all rotations and reflections of the given solution. -}
allRotationsAndReflectionsOf ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolutions {-^ all rotations and reflections of the given solution -}
allRotationsAndReflectionsOf = flip multiplySolution NoSymmetries

{-| Computes all rotations of the given solution. -}
allRotationsOf ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolutions {-^ all rotations of the given solution -}
allRotationsOf n = take 4 . iterate (rotateLeft n)

{-| Converts coordinates of type 'Int' to type 'Word'. -}
convertSolutionToWord :: [(Int,Int)] → [(Word,Word)]
convertSolutionToWord = map (fromIntegral *** fromIntegral)

{-| Extracts the outermost layers of a solution. -}
extractExteriorFromSolution ::
    Word {-^ board size -} →
    Word {-^ number of outer layers to extract -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolution {-^ the outermost layers of the solution -}
extractExteriorFromSolution size layers = filter . uncurry $ ((||) `on` (liftA2 (||) (< threshold_1) (> threshold_2)))
  where
    threshold_1 = layers
    threshold_2 = size-layers-1

{-| Get the openings for a queen -}
getOpenings ::
    MonadPlus m ⇒
    Int {-^ board size -} →
    Word64 {-^ occupied positions -} →
    m PositionAndBit {-^ open positions and their corresponding bits -}
getOpenings size blocked
    | blocked .&. mask == mask = mzero
    | otherwise = go emptyForest (PositionAndBit 0 1)
  where
    mask = bit size - 1
    go !forest !x@(PositionAndBit i b)
     | i >= size          = consolidateForest forest
     | b .&. blocked == 0 = go (addToForest forest (return x)) next_x
     | otherwise          = go forest next_x
     where
       next_x = PositionAndBit (i+1) (b `unsafeShiftL` 1)
{-# INLINE getOpenings #-}

{-| Get the openings for a queen -}
getOpeningsAsList ::
    Int {-^ board size -} →
    Word64 {-^ occupied positions -} →
    [PositionAndBit] {-^ open positions and their corresponding bits as a list -}
getOpeningsAsList size blocked
    | blocked .&. mask == mask = []
    | otherwise = go $ PositionAndBit 0 1
  where
    mask = bit size - 1
    go !x@(PositionAndBit i b)
     | i >= size          = []
     | b .&. blocked == 0 = x:go next_x
     | otherwise          = go next_x
     where
       next_x = PositionAndBit (i+1) (b `unsafeShiftL` 1)
{-# INLINE getOpeningsAsList #-}

{-| Get the symmetric openings for a queen -}
getSymmetricOpenings ::
    MonadPlus m ⇒
    Int {-^ board size -} →
    Word64 {-^ occupied positions -} →
    m PositionAndBitWithReflection {-^ open positions and their corresponding bits and reflections -}
getSymmetricOpenings size blocked
    | blocked .&. mask == mask = mzero
    | otherwise = go emptyForest (PositionAndBitWithReflection 0 1 end end_bit)
  where
    end = size-1
    end_bit = bit end
    mask = bit size - 1
    go forest x@(PositionAndBitWithReflection i b ri rb)
     | i >= ri            = consolidateForest forest
     | b .&. blocked == 0 = go (addToForest (addToForest forest (return x)) (return $ PositionAndBitWithReflection ri rb i b)) next_bit
     | otherwise          = go forest next_bit
     where
       next_bit = PositionAndBitWithReflection (i+1) (b `unsafeShiftL` 1) (ri-1) (rb `unsafeShiftR` 1)
{-# INLINE getSymmetricOpenings #-}

{-| Checks if a solution has reflection symmetry. -}
hasReflectionSymmetry ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    Bool {-^ true if the given solution has reflection symmetry -}
hasReflectionSymmetry n = liftA2 ((==) `on` sort) id (reflectSolution n)

{-| Checks if a solution has 90-degree rotation symmetry. -}
hasRotate90Symmetry ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    Bool {-^ true if the given solution has 90-degree rotation symmetry -}
hasRotate90Symmetry n = liftA2 ((==) `on` sort) id (rotateLeft n)

{-| Checks if a solution has 180-degree rotation symmetry. -}
hasRotate180Symmetry ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    Bool {-^ true if the given solution has 180-degree rotation symmetry -}
hasRotate180Symmetry n = liftA2 ((==) `on` sort) id (rotate180 n)

{-| Returns the number of equivalent solutions for a solution with a given symmetry. -}
multiplicityForSymmetry :: NQueensSymmetry → Word
multiplicityForSymmetry AllSymmetries = 1
multiplicityForSymmetry AllRotations = 2
multiplicityForSymmetry Rotate180Only = 4
multiplicityForSymmetry NoSymmetries = 8
{-# INLINE multiplicityForSymmetry #-}

{-| Gets all of the equivalent solutions with an equivalent symmetry. -}
multiplySolution :: MonadPlus m ⇒
    Word {-^ board size -} →
    NQueensSymmetry {-^ the symmetry of the solution -} →
    NQueensSolution {-^ a solution with the given symmetry -} →
    m NQueensSolution {-^ the equivalent solutions of the given solution -}
multiplySolution n = go
  where
    go AllSymmetries = return
    go AllRotations = liftM2 (mplus `on` return) id (reflectSolution n) >=> go AllSymmetries
    go Rotate180Only = liftM2 (mplus `on` return) id (rotateLeft n) >=> go AllRotations
    go NoSymmetries = liftM2 (mplus `on` return) id (rotate180 n) >=> go Rotate180Only
{-# INLINE multiplySolution #-}

{-| Reflects the bits in a number so that each bit at position i is moved to
    position -i (i.e., what you get when you take a bit at position 0 and rotate
    it i positions to the right)
 -}
reflectBits :: Word64 → Word64
reflectBits = go 0 (0::Int) 1
  where
    go !accum 64 _ _ = accum
    go !accum !column !column_bit !bits =
        go (accum + column_bit * (bits .&. 1))
           (column + 1)
           (column_bit `unsafeShiftL` 1)
           (bits `rotateL` 1)

{-| Reflects the columns of a solution -}
reflectSolution ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolution {-^ the solution with its columns reflected -}
reflectSolution n old_solution = map (\(row,col) → (row,last-col)) old_solution
  where
    last = n - 1

{-| Rotate a solution left by 180 degrees. -}
rotate180 ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolution {-^ the given solution rotated by 180 degrees -}
rotate180 n = map (\(row,col) → (last-row,last-col))
  where
    last = n - 1

{-| Rotate a solution left by 90 degrees. -}
rotateLeft ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolution {-^ the given solution rotated left by 90 degrees -}
rotateLeft n = map (\(row,col) → (col,last-row))
  where
    last = n - 1

{-| Rotate a solution right by 90 degrees. -}
rotateRight ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSolution {-^ the given solution rotated right by 90 degrees -}
rotateRight n = map (\(row,col) → (last-col,row))
  where
    last = n - 1

{-| Computes the symmetry class of the given solution -}
symmetryOf ::
    Word {-^ board size -} →
    NQueensSolution {-^ given solution -} →
    NQueensSymmetry {-^ the symmetry of the given solution -}
symmetryOf n solution
  | hasReflectionSymmetry n solution = AllSymmetries
  | hasRotate90Symmetry n solution = AllRotations
  | hasRotate180Symmetry n solution = Rotate180Only
  | otherwise = NoSymmetries


