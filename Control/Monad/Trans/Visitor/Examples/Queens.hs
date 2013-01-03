-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE ScopedTypeVariables #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens
    (nqueens_correct_counts
    ,nqueensCorrectCount
    ,nqueensCount
    ,nqueensSolutions
    ,nqueensTrivial
    ) where

-- Imports {{{
import Control.Monad (MonadPlus(..))
import Data.Bits ((.&.),(.|.),bit,unsafeShiftL,unsafeShiftR)
import Data.List (sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Word (Word64)

import Control.Monad.Trans.Visitor (Visitor,allFromGreedy)
-- }}}

-- Types {{{

data Side = TOP | RIGHT | BOTTOM | LEFT deriving (Eq,Ord,Read,Show)

data NQueensCallbacks α β = NQueensCallbacks -- {{{
    {   updateValue :: !((Int,Int) → α → α)
    ,   finalizeValue :: !(α → β)
    }
-- }}}

data NQueensSearchState = NQueensSearchState -- {{{
    {   number_of_queens_remaining :: {-# UNPACK #-} !Int
    ,   window :: {-# UNPACK #-} !NQueensWindow
    ,   occupied :: {-# UNPACK #-} !NQueensOccupied
    }
-- }}}

data NQueensOccupied = NQueensOccupied -- {{{
    {   occupied_rows :: {-# UNPACK #-} !Word64
    ,   occupied_columns :: {-# UNPACK #-} !Word64
    ,   occupied_top_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_top_positive_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_right_positive_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_bottom_negative_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_bottom_positive_diagonals :: {-# UNPACK #-} !Word64
    ,   occupied_left_positive_diagonals :: {-# UNPACK #-} !Word64
    } deriving Show
-- }}}

data NQueensWindow = NQueensWindow -- {{{
    {   first :: {-# UNPACK #-} !Int
    ,   last :: {-# UNPACK #-} !Int
    ,   window_size :: {-# UNPACK #-} !Int
    ,   window_end_bit :: {-# UNPACK #-} !Word64
    } deriving Show
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

getOpenings :: MonadPlus m ⇒ Int → Word64 → Int → Word64 → m (Int,Word64) -- {{{
getOpenings start start_bit end blocked
    | blocked .&. mask == mask = mzero
    | otherwise = allFromGreedy $ go start start_bit
  where
    mask = (start_bit `unsafeShiftL` (end-start+1)) - 1
    go !i !b
     | i > end              =       []
     | (b .&. blocked == 0) = (i,b):next
     | otherwise            =       next
     where
       next = go (i+1) (b `unsafeShiftL` 1)
{-# INLINE getOpenings #-}
-- }}}

narrowOccupied :: NQueensOccupied → NQueensOccupied -- {{{
narrowOccupied NQueensOccupied{..} =
    NQueensOccupied
        (occupied_rows `unsafeShiftR` 1)
        (occupied_columns `unsafeShiftR` 1)
        (occupied_top_negative_diagonals `unsafeShiftR` 2)
         occupied_top_positive_diagonals
         occupied_right_positive_diagonals
         occupied_bottom_negative_diagonals
        (occupied_bottom_positive_diagonals `unsafeShiftR` 2)
        (occupied_left_positive_diagonals `unsafeShiftR` 2)
-- }}}

narrowWindow :: NQueensWindow → NQueensWindow -- {{{
narrowWindow NQueensWindow{..} =
    NQueensWindow
        (first+1)
        (last-1)
        (window_size-2)
        (window_end_bit `unsafeShiftR` 2)
{-# INLINE narrowWindow #-}
-- }}}

nqueensCorrectCount :: Int → Int -- {{{
nqueensCorrectCount = fromJust . ($ nqueens_correct_counts) . IntMap.lookup
-- }}}

nqueensGeneric :: (MonadPlus m, Show α) ⇒ α → NQueensCallbacks α β → Int → m β -- {{{
-- nqueensGeneric initial_value NQueensCallbacks{..} 1 = return . finalizeValue . updateValue (0,0) $ initial_value
-- nqueensGeneric initial_value NQueensCallbacks{..} 2 = mzero
-- nqueensGeneric initial_value NQueensCallbacks{..} 3 = mzero
nqueensGeneric initial_value callbacks@NQueensCallbacks{..} n =
    nqueensSearch callbacks initial_value n 0 (n-1) (NQueensOccupied 0 0 0 0 0 0 0 0)
{-# INLINE nqueensGeneric #-}
-- }}}

nqueensSearch :: ∀ α β m. (MonadPlus m, Show α) ⇒ NQueensCallbacks α β → α → Int → Int → Int → NQueensOccupied → m β -- {{{
nqueensSearch NQueensCallbacks{..} value 0 _ _ _ = return $ finalizeValue value
nqueensSearch NQueensCallbacks{..} value number_of_queens_remaining first last occupied =
    go value $
        NQueensSearchState
            number_of_queens_remaining
            (NQueensWindow first last (last-first+1) (bit last))
            occupied
  where
    go :: α → NQueensSearchState → m β
    go !value !(NQueensSearchState{occupied=occupied@NQueensOccupied{..},window=window@NQueensWindow{..},..})
      | window_size < number_of_queens_remaining = mzero
      | window_size == 1 = -- {{{
         if number_of_queens_remaining == 1 && occupied_rows .&. 1 .|. occupied_columns .&. 1 .|. occupied_top_positive_diagonals .&. 1 .|. occupied_top_negative_diagonals .&. 1 == 0
            then return $ finalizeValue ((first,first) `updateValue` value)
            else mzero
        -- }}}
      | window_size == 2 = -- {{{
         if number_of_queens_remaining == 1
            then (if positive_corners_are_available
                    then (if occupied_rows .&. 1 .|. occupied_columns .&. 1 .|. occupied_top_negative_diagonals .&. 1 == 0 then return (first,first) else mzero) `mplus`
                         (if occupied_rows .&. 2 .|. occupied_columns .&. 2 .|. occupied_bottom_negative_diagonals .&. 2 == 0 then return (last,last) else mzero)
                    else mzero
                 ) `mplus`
                 (if negative_corners_are_available
                    then (if occupied_rows .&. 1 .|. occupied_columns .&. 2 .|. occupied_top_positive_diagonals .&. 2 == 0 then return (first,last) else mzero) `mplus`
                         (if occupied_rows .&. 2 .|. occupied_columns .&. 1 .|. occupied_bottom_positive_diagonals .&. 1 == 0 then return (last,first) else mzero)
                    else mzero
                 ) >>= return . finalizeValue . flip updateValue value
            else mzero
        -- }}}
      | window_size == 3 && number_of_queens_remaining == 3 = mzero
      | negative_corners_are_available = -- {{{
            if positive_corners_are_available
                then placeNegativeCorner `mplus` maybePlaceAllSides `mplus` placePositiveCorner
                else placeNegativeCorner `mplus` maybePlaceAllSides
        -- }}}
      | positive_corners_are_available = placePositiveCorner `mplus` maybePlaceAllSides
      | otherwise = maybePlaceAllSides
      where
        negative_corners_are_available = occupied_bottom_negative_diagonals .&. 1 == 0
        positive_corners_are_available = occupied_top_positive_diagonals .&. 1 == 0

        maybePlaceAllSides = maybePlaceSides number_of_queens_remaining value [TOP,RIGHT,BOTTOM,LEFT] occupied

        placeCorner :: -- {{{
            Bool → Int → Word64 → Int → Word64 → [Side] →
            Bool → Int → Word64 → Int → Word64 → [Side] →
            (Word64 → Word64 → NQueensOccupied) →
            m β
        placeCorner
          !c1_available !c1_row !c1_row_bit !c1_column !c1_column_bit !c1_sides
          !c2_available !c2_row !c2_row_bit !c2_column !c2_column_bit !c2_sides
          !computeOccupied
          | c1_available =
             if c2_available
                then placeC1 `mplus` placeC2
                else placeC1
          | c2_available = placeC2
          | otherwise = mzero
          where
            placeC1 = placeAt c1_row c1_row_bit c1_column c1_column_bit c1_sides
            placeC2 = placeAt c2_row c2_row_bit c2_column c2_column_bit c2_sides

            placeAt :: Int → Word64 → Int → Word64 → [Side] → m β
            placeAt !row !row_bit !column !column_bit !sides =
                maybePlaceSides
                    (number_of_queens_remaining-1)
                    ((row,column) `updateValue` value)
                     sides
                    (computeOccupied row_bit column_bit)
        -- }}}

        placePositiveCorner = -- {{{
            placeCorner
                (occupied_rows .&. 1 .|. occupied_columns .&. 1 .|. occupied_top_negative_diagonals .&. 1 == 0)
                first 1 first 1 [BOTTOM,LEFT]
                (occupied_rows .&. window_end_bit .|. occupied_columns .&. window_end_bit .|. occupied_bottom_negative_diagonals .&. window_end_bit == 0)
                last window_end_bit last window_end_bit [TOP,RIGHT]
            $ \row_bit column_bit →
                NQueensOccupied
                    (occupied_rows .|. row_bit)
                    (occupied_columns .|. column_bit)
                     occupied_top_negative_diagonals
                    (occupied_top_positive_diagonals .|. 1)
                    (occupied_right_positive_diagonals .|. 1)
                     occupied_bottom_negative_diagonals
                    (occupied_bottom_positive_diagonals .|. window_end_bit)
                    (occupied_left_positive_diagonals .|. window_end_bit)
        -- }}}

        placeNegativeCorner = -- {{{
            placeCorner
                (occupied_rows .&. 1 .|. occupied_columns .&. window_end_bit .|. occupied_top_positive_diagonals .&. window_end_bit == 0)
                first 1 last window_end_bit [RIGHT,BOTTOM]
                (occupied_rows .&. window_end_bit .|. occupied_columns .&. 1 .|. occupied_bottom_positive_diagonals .&. 1 == 0)
                last window_end_bit first 1 [LEFT,TOP]
            $ \row_bit column_bit →
                NQueensOccupied
                    (occupied_rows .|. row_bit)
                    (occupied_columns .|. column_bit)
                    (occupied_top_negative_diagonals .|. window_end_bit)
                     occupied_top_positive_diagonals
                     occupied_right_positive_diagonals
                    (occupied_bottom_negative_diagonals .|. 1)
                     occupied_bottom_positive_diagonals
                     occupied_left_positive_diagonals
        -- }}}

        window_shift = window_size-1
        reflect = (-) last
        bitReflect = bit . reflect

        maybePlaceSides :: Int → α → [Side] → NQueensOccupied → m β -- {{{
        maybePlaceSides 0 !value _ _ = return $ finalizeValue value
        maybePlaceSides !number_of_queens_remaining !value [] !occupied = -- {{{
            go value $
                NQueensSearchState
                    number_of_queens_remaining
                    (narrowWindow window)
                    (narrowOccupied occupied)
        -- }}}
        maybePlaceSides !number_of_queens_remaining !value (side:rest_sides) occupied@NQueensOccupied{..} = -- {{{
            case side of
                TOP -- {{{
                  | occupied_rows .&. 1 /= 0 → nextSide
                  | otherwise →
                     maybePlaceSideAt
                        (occupied_columns .|. occupied_top_negative_diagonals .|. occupied_top_positive_diagonals)
                        (\value column → (first,column) `updateValue` value)
                        (\column column_bit → NQueensOccupied
                            (occupied_rows .|. 1)
                            (occupied_columns .|. column_bit)
                            (occupied_top_negative_diagonals .|. column_bit)
                            (occupied_top_positive_diagonals .|. column_bit)
                             occupied_right_positive_diagonals
                             occupied_bottom_negative_diagonals
                             occupied_bottom_positive_diagonals
                            (occupied_left_positive_diagonals .|. bitReflect column)
                        )
                -- }}}
                RIGHT -- {{{
                 | occupied_columns .&. 1 /= 0 → nextSide
                 | otherwise →
                     maybePlaceSideAt
                        (occupied_rows .|. occupied_top_negative_diagonals .|. occupied_right_positive_diagonals)
                        (\value row → (row,first) `updateValue` value)
                        (\row row_bit → NQueensOccupied
                            (occupied_rows .|. row_bit)
                            (occupied_columns .|. 1)
                            (occupied_top_negative_diagonals .|. row_bit)
                             occupied_top_positive_diagonals
                            (occupied_right_positive_diagonals .|. row_bit)
                             occupied_bottom_negative_diagonals
                            (occupied_bottom_positive_diagonals .|. bitReflect row)
                             occupied_left_positive_diagonals
                        )
                -- }}}
                BOTTOM -- {{{
                 | occupied_rows .&. window_end_bit /= 0 → nextSide
                 | otherwise →
                     maybePlaceSideAt
                        (occupied_columns .|. occupied_bottom_negative_diagonals .|. occupied_bottom_positive_diagonals)
                        (\value column → (last,column) `updateValue` value)
                        (\column column_bit → NQueensOccupied
                            (occupied_rows .|. window_end_bit)
                            (occupied_columns .|. column_bit)
                             occupied_top_negative_diagonals
                             occupied_top_positive_diagonals
                            (occupied_right_positive_diagonals .|. bitReflect column)
                            (occupied_bottom_negative_diagonals .|. column_bit)
                            (occupied_bottom_positive_diagonals .|. column_bit)
                             occupied_left_positive_diagonals
                        )
                -- }}}
                LEFT -- {{{
                 | occupied_columns .&. window_end_bit /= 0 → nextSide
                 | otherwise →
                     maybePlaceSideAt
                        (occupied_rows .|. occupied_bottom_negative_diagonals .|. occupied_left_positive_diagonals)
                        (\value row → (row,last) `updateValue` value)
                        (\row row_bit → NQueensOccupied
                            (occupied_rows .|. row_bit)
                            (occupied_columns .|. window_end_bit)
                             occupied_top_negative_diagonals
                            (occupied_top_positive_diagonals .|. bitReflect row)
                             occupied_right_positive_diagonals
                            (occupied_bottom_negative_diagonals .|. row_bit)
                             occupied_bottom_positive_diagonals
                            (occupied_left_positive_diagonals .|. row_bit)
                        )
                -- }}}
          where
            nextSide = maybePlaceSides number_of_queens_remaining value rest_sides occupied

            maybePlaceSideAt blocked updateValueWith computeNewOccupied = -- {{{
                getOpenings (first+1) 2 (last-1) blocked
                >>=
                \(i,b) →
                    maybePlaceSides
                        (number_of_queens_remaining-1)
                        (value `updateValueWith` i)
                         rest_sides
                        (computeNewOccupied i b)
            -- }}}
        -- }}}
        -- }}}
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