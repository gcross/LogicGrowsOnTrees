-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Examples.Queens.Implementation where

-- Imports {{{
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

import Visitor (Visitor,between)
import Visitor.Utils.MonadStacks (addToStacks,emptyStacks,mergeStacks)
import Visitor.Utils.WordSum
-- }}}

-- Types {{{

data NQueensSymmetry =
    NoSymmetries
  | Rotate180Only
  | AllRotations
  | AllSymmetries
  deriving (Eq,Ord,Read,Show)

data NQueensBreak90State = NQueensBreak90State -- {{{
    {   b90_number_of_queens_remaining :: {-# UNPACK #-} !Word
    ,   b90_window_start :: {-# UNPACK #-} !Int
    ,   b90_window_size :: {-# UNPACK #-} !Int
    ,   b90_occupied_rows_and_columns :: {-# UNPACK #-} !Word64
    ,   b90_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   b90_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }
-- }}}

data NQueensBreak180State = NQueensBreak180State -- {{{
    {   b180_number_of_queens_remaining :: {-# UNPACK #-} !Word
    ,   b180_window_start :: {-# UNPACK #-} !Int
    ,   b180_window_size :: {-# UNPACK #-} !Int
    ,   b180_occupied_rows :: {-# UNPACK #-} !Word64
    ,   b180_occupied_columns :: {-# UNPACK #-} !Word64
    ,   b180_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   b180_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    ,   b180_occupied_right_positive_diagonals :: {-# UNPACK #-} !Word64
    }
-- }}}

data NQueensSearchState = NQueensSearchState -- {{{
    {   s_number_of_queens_remaining :: {-# UNPACK #-} !Word
    ,   s_row :: {-# UNPACK #-} !Int
    ,   s_occupied_rows :: {-# UNPACK #-} !Word64
    ,   s_occupied_columns :: {-# UNPACK #-} !Word64
    ,   s_occupied_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   s_occupied_positive_diagonals :: {-# UNPACK #-} !Word64
    }
-- }}}

type NQueensSolution = [(Word,Word)]
type NQueensSolutions = [NQueensSolution]

data PositionAndBit = PositionAndBit {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
data PositionAndBitWithReflection = PositionAndBitWithReflection {-# UNPACK #-} !Int {-# UNPACK #-} !Word64  {-# UNPACK #-} !Int {-# UNPACK #-} !Word64

-- }}}

-- Foreign Functions {{{

foreign import ccall safe "queens.h Visitor_Queens_count_solutions" c_Visitor_Queens_count_solutions ::
    CUInt →
    CUInt →
    CUInt →
    Word64 →
    Word64 →
    Word64 →
    Word64 →
    FunPtr (CUInt -> CUInt → IO ()) →
    FunPtr (IO ()) →
    FunPtr (IO ()) →
    IO CUInt

foreign import ccall "wrapper" mkPushValue :: (CUInt → CUInt → IO ()) → IO (FunPtr (CUInt → CUInt → IO ()))
foreign import ccall "wrapper" mkPopValue :: IO () → IO (FunPtr (IO ()))
foreign import ccall "wrapper" mkFinalizeValue :: IO () → IO (FunPtr (IO ()))

nqueensCSearch ::
    ∀ α m β.
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) →
    (α → m β) →
    α →
    Int →
    Int →
    NQueensSearchState → m β
nqueensCSearch _ finalizeValue value _ _ NQueensSearchState{s_number_of_queens_remaining=0} = finalizeValue value
nqueensCSearch updateValue finalizeValue value size window_start NQueensSearchState{..}
  | typeOf value == typeOf () && typeOf (undefined :: β) == typeOf (undefined :: WordSum) = do
        Just (WordSum multiplier) ← liftM cast (finalizeValue value)
        let number_found =
                fromIntegral
                .
                unsafePerformIO
                $
                c_Visitor_Queens_count_solutions
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
        _ ← c_Visitor_Queens_count_solutions
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
-- }}}

-- Functions {{{

allRotationsAndReflectionsOf :: Word → NQueensSolution → NQueensSolutions -- {{{
allRotationsAndReflectionsOf = flip multiplySolution NoSymmetries
-- }}}

allRotationsOf :: Word → NQueensSolution → NQueensSolutions -- {{{
allRotationsOf n = take 4 . iterate (rotateLeft n)
-- }}}

convertSolutionToWord :: [(Int,Int)] → [(Word,Word)] -- {{{
convertSolutionToWord = map (fromIntegral *** fromIntegral)
-- }}}

extractExteriorFromSolution :: Word → Word → NQueensSolution → NQueensSolution -- {{{
extractExteriorFromSolution size layers = filter . uncurry $ ((||) `on` (liftA2 (||) (< threshold_1) (> threshold_2)))
  where
    threshold_1 = layers
    threshold_2 = size-layers-1
-- }}}

getOpenings :: MonadPlus m ⇒ Int → Word64 → m PositionAndBit -- {{{
getOpenings size blocked
    | blocked .&. mask == mask = mzero
    | otherwise = go emptyStacks (PositionAndBit 0 1)
  where
    mask = bit size - 1
    go !stacks !x@(PositionAndBit i b)
     | i >= size          = mergeStacks stacks
     | b .&. blocked == 0 = go (addToStacks stacks (return x)) next_x
     | otherwise          = go stacks next_x
     where
       next_x = PositionAndBit (i+1) (b `unsafeShiftL` 1)
{-# INLINE getOpenings #-}
-- }}}

getOpeningsAsList :: Int → Word64 → [PositionAndBit] -- {{{
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
-- }}}

getSymmetricOpenings :: MonadPlus m ⇒ Int → Word64 → m PositionAndBitWithReflection -- {{{
getSymmetricOpenings size blocked
    | blocked .&. mask == mask = mzero
    | otherwise = go emptyStacks (PositionAndBitWithReflection 0 1 end end_bit)
  where
    end = size-1
    end_bit = bit end
    mask = bit size - 1
    go stacks x@(PositionAndBitWithReflection i b ri rb)
     | i >= ri            = mergeStacks stacks
     | b .&. blocked == 0 = go (addToStacks (addToStacks stacks (return x)) (return $ PositionAndBitWithReflection ri rb i b)) next_bit
     | otherwise          = go stacks next_bit
     where
       next_bit = PositionAndBitWithReflection (i+1) (b `unsafeShiftL` 1) (ri-1) (rb `unsafeShiftR` 1)
{-# INLINE getSymmetricOpenings #-}
-- }}}

hasReflectionSymmetry :: Word → NQueensSolution → Bool -- {{{
hasReflectionSymmetry n = liftA2 ((==) `on` sort) id (reflectSolution n)
-- }}}

hasRotate90Symmetry :: Word → NQueensSolution → Bool -- {{{
hasRotate90Symmetry n = liftA2 ((==) `on` sort) id (rotateLeft n)
-- }}}

hasRotate180Symmetry :: Word → NQueensSolution → Bool -- {{{
hasRotate180Symmetry n = liftA2 ((==) `on` sort) id (rotate180 n)
-- }}}

multiplicityForSymmetry :: NQueensSymmetry → Word -- {{{
multiplicityForSymmetry AllSymmetries = 1
multiplicityForSymmetry AllRotations = 2
multiplicityForSymmetry Rotate180Only = 4
multiplicityForSymmetry NoSymmetries = 8
{-# INLINE multiplicityForSymmetry #-}
-- }}}

multiplySolution :: MonadPlus m ⇒ Word → NQueensSymmetry → NQueensSolution → m NQueensSolution -- {{{
multiplySolution n = go
  where
    go AllSymmetries = return
    go AllRotations = liftM2 (mplus `on` return) id (reflectSolution n) >=> go AllSymmetries
    go Rotate180Only = liftM2 (mplus `on` return) id (rotateLeft n) >=> go AllRotations
    go NoSymmetries = liftM2 (mplus `on` return) id (rotate180 n) >=> go Rotate180Only
{-# INLINE multiplySolution #-}
-- }}}

nqueensBreak90 :: -- {{{
    MonadPlus m ⇒
    ([(Word,Word)] → α → α) →
    (α → m β) →
    (α → NQueensBreak90State → m β) →
    (α → NQueensBreak180State → m β) →
    (α → Int → NQueensSearchState → m β) →
    α →
    NQueensBreak90State →
    m β
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
    keep90 = do -- {{{
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
    -- }}}
    breakTo180 = do -- {{{
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
    -- }}}
    breakAtCorner = do -- {{{
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
    -- }}}
    breakAtSides = do -- {{{
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
    -- }}}
    nextWindow = break90 value $ -- {{{
        NQueensBreak90State
             number_of_queens_remaining
            (window_start+1)
            (window_size-2)
            (occupied_rows_and_columns `unsafeShiftR` 1)
            (occupied_negative_diagonals `unsafeShiftR` 2)
             occupied_positive_diagonals
    -- }}}
-- }}}

nqueensBreak180 :: -- {{{
    MonadPlus m ⇒
    ([(Word,Word)] → α → α) →
    (α → m β) →
    (α → NQueensBreak180State → m β) →
    (α → Int → NQueensSearchState → m β) →
    α →
    NQueensBreak180State →
    m β
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
    breakAtSides = do -- {{{
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
    -- }}}
    breakAtHorizontalSides = do -- {{{
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
    -- }}}
    breakAtVerticalSides = do -- {{{
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
    -- }}}
    breakAtBottomLeftCorner = do -- {{{
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
    -- }}}
    breakAtTopLeftCorner = do -- {{{
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
    -- }}}
    preserve180 = do -- {{{
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
    -- }}}
    preserve180Horizontal = do -- {{{
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
    -- }}}
    preserve180Vertical = do -- {{{
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
    -- }}}
    nextWindow = break180 value $ -- {{{
        NQueensBreak180State
             number_of_queens_remaining
            (window_start+1)
            (window_size-2)
            (occupied_rows `unsafeShiftR` 1)
            (occupied_columns `unsafeShiftR` 1)
            (occupied_negative_diagonals `unsafeShiftR` 2)
             occupied_positive_diagonals
             occupied_right_positive_diagonals
    -- }}}
-- }}}

nqueensBruteForceGeneric :: -- {{{
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) →
    (α → m β) →
    α →
    Word →
    m β
nqueensBruteForceGeneric updateValue finalizeValue initial_value 1 = finalizeValue . updateValue [(0,0)] $ initial_value
nqueensBruteForceGeneric _ _ _ 2 = mzero
nqueensBruteForceGeneric _ _ _ 3 = mzero
nqueensBruteForceGeneric updateValue finalizeValue initial_value n = nqueensSearch updateValue finalizeValue initial_value (fromIntegral n) $ NQueensSearchState n 0 0 0 0 0
{-# INLINE nqueensBruteForceGeneric #-}
-- }}}

nqueensBruteForceCount :: MonadPlus m ⇒ Word → m WordSum -- {{{
nqueensBruteForceCount = nqueensBruteForceGeneric (const id) (const . return $ WordSum 1) ()
{-# SPECIALIZE nqueensBruteForceCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensBruteForceCount :: Word → Visitor WordSum #-}
-- }}}

nqueensBruteForceSolutions :: MonadPlus m ⇒ Word → m NQueensSolution -- {{{
nqueensBruteForceSolutions = nqueensBruteForceGeneric (++) return []
{-# SPECIALIZE nqueensBruteForceSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensBruteForceSolutions :: Word → Visitor NQueensSolution #-}
-- }}}

nqueensCGeneric :: -- {{{
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) →
    (α → m β) →
    α →
    Word →
    m β
nqueensCGeneric updateValue finalizeValue initial_value 1 =
    finalizeValue . updateValue [(0,0)] $ initial_value
nqueensCGeneric _ _ _ 2 = mzero
nqueensCGeneric _ _ _ 3 = mzero
nqueensCGeneric updateValue finalizeValue initial_value n =
    nqueensCSearch updateValue finalizeValue initial_value (fromIntegral n) 0 $
        NQueensSearchState n 0 0 0 0 0
{-# INLINE nqueensCGeneric #-}
-- }}}

nqueensCCount :: MonadPlus m ⇒ Word → m WordSum -- {{{
nqueensCCount = nqueensCGeneric (const id) (const . return $ WordSum 1) ()
{-# SPECIALIZE nqueensCCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensCCount :: Word → Visitor WordSum #-}
-- }}}

nqueensCSolutions :: MonadPlus m ⇒ Word → m NQueensSolution -- {{{
nqueensCSolutions = nqueensCGeneric (++) return []
{-# SPECIALIZE nqueensCSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensCSolutions :: Word → Visitor NQueensSolution #-}
-- }}}

nqueensGeneric :: -- {{{
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) →
    (Word → NQueensSymmetry → α → m β) →
    α →
    Word →
    m β
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
-- }}}

nqueensSearch :: -- {{{
    (MonadPlus m
    ,Typeable α
    ,Typeable β
    ) ⇒
    ([(Word,Word)] → α → α) →
    (α → m β) →
    α →
    Int →
    NQueensSearchState →
    m β
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
-- }}}

nqueensStart :: -- {{{
    MonadPlus m ⇒
    ([(Word,Word)] → α → α) →
    (α → NQueensBreak90State → m β) →
    (α → NQueensBreak180State → m β) →
    (α → Int → NQueensSearchState → m β) →
    α →
    Word →
    m β
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
    preserve90 = do -- {{{
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
    -- }}}
    breakTo180 = do -- {{{
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
    -- }}}
    breakAtCorner = do -- {{{
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
    -- }}}
    breakAtSides = do -- {{{
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
    -- }}}
-- }}}

reflectBits :: Word64 → Word64 -- {{{
reflectBits = go 0 (0::Int) 1
  where
    go !accum 64 _ _ = accum
    go !accum !column !column_bit !bits =
        go (accum + column_bit * (bits .&. 1))
           (column + 1)
           (column_bit `unsafeShiftL` 1)
           (bits `rotateL` 1)
-- }}}

reflectSolution :: Word → NQueensSolution → NQueensSolution -- {{{
reflectSolution n old_solution = map (\(row,col) → (row,last-col)) old_solution
  where
    last = n - 1
-- }}}

rotate180 :: Word → NQueensSolution → NQueensSolution -- {{{
rotate180 n = map (\(row,col) → (last-row,last-col))
  where
    last = n - 1
-- }}}

rotateLeft :: Word → NQueensSolution → NQueensSolution -- {{{
rotateLeft n = map (\(row,col) → (col,last-row))
  where
    last = n - 1
-- }}}

rotateRight :: Word → NQueensSolution → NQueensSolution -- {{{
rotateRight n = map (\(row,col) → (last-col,row))
  where
    last = n - 1
-- }}}

symmetryOf :: Word → NQueensSolution → NQueensSymmetry -- {{{
symmetryOf n solution
  | hasReflectionSymmetry n solution = AllSymmetries
  | hasRotate90Symmetry n solution = AllRotations
  | hasRotate180Symmetry n solution = Rotate180Only
  | otherwise = NoSymmetries
-- }}}

-- }}}
