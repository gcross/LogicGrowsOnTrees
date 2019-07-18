{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains examples of logic programs that generate solutions to the
    n-queens problem, which is the problem of finding ways to put n queens on an
    n x n chessboard in such a way that they do not conflict. Solutions of the
    n-queens problem take the form of a list of n coordinates such that no
    coordinates have overlapping rows, columns, or diagonals (as these are the
    directions in which a queen can attack).
 -}
module LogicGrowsOnTrees.Examples.Queens
    (
    -- * Correct solution counts
      nqueens_correct_counts
    , nqueens_maximum_size
    , nqueensCorrectCount
    -- * Basic examples
    -- $basic

    -- ** Using sets
    -- $sets
    , nqueensUsingSetsSolutions
    , nqueensUsingSetsCount
    -- ** Using bits
    , nqueensUsingBitsSolutions
    , nqueensUsingBitsCount
    -- * Advanced example
    -- $advanced
    , nqueensGeneric
    , nqueensSolutions
    , nqueensCount
    -- ** With a list at the bottom (instead of C)
    ,nqueensWithListAtBottomGeneric
    ,nqueensWithListAtBottomSolutions
    ,nqueensWithListAtBottomCount
    -- ** With nothing at the bottom (instead of C or a List)
    ,nqueensWithNothingAtBottomGeneric
    ,nqueensWithNothingAtBottomSolutions
    ,nqueensWithNothingAtBottomCount
    -- * Board size command argument
    , boardSizeArgument
    ) where

import Control.Monad (MonadPlus,(>=>),guard,liftM)

import Data.Bits ((.|.),(.&.),bit,finiteBitSize,shiftL,shiftR)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import Data.Word (Word,Word64)

import Options.Applicative (Parser,ReadM,argument,eitherReader,help,metavar)

import Text.Read (readMaybe)
import Text.Printf (printf)

import LogicGrowsOnTrees (Tree,allFrom,exploreTree) -- exploreTree added so that haddock will link to it
import qualified LogicGrowsOnTrees.Examples.Queens.Advanced as Advanced
import LogicGrowsOnTrees.Examples.Queens.Advanced
    (NQueensSolution
    ,NQueensSolutions
    ,multiplySolution
    ,nqueensGeneric
    ,nqueensWithListAtBottomGeneric
    ,nqueensWithNothingAtBottomGeneric
    )
import LogicGrowsOnTrees.Utils.WordSum

--------------------------------------------------------------------------------
---------------------------------- Board size ----------------------------------
--------------------------------------------------------------------------------

{-| The argument for specifying the board size. -}
boardSizeArgument ∷ Parser Word
boardSizeArgument =
   argument argumentReader $ metavar "BOARD_SIZE" <> help "board size"
  where
      argumentReader ∷ ReadM Word
      argumentReader = eitherReader $
        maybe
            (fail "board size was not a valid number")
            pure
        .
        readMaybe
        >=>
        (\n → if n >= 1 && n <= fromIntegral nqueens_maximum_size
            then pure n
            else fail $ printf
                "bad board size %i %i (must be between 1 and %i inclusive)"
                (show n) (show n)
                (show nqueens_maximum_size)
        )

--------------------------------------------------------------------------------
-------------------------------- Correct counts --------------------------------
--------------------------------------------------------------------------------

{-| A table with the correct number of solutions for board sizes ranging from 1
    to `nqueens_maximum_size`.

    This data was pulled from <http://queens.inf.tu-dresden.de/?n=f&l=en>.
 -}
nqueens_correct_counts :: IntMap Word
nqueens_correct_counts = IntMap.fromDistinctAscList $
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
    ] ++ if finiteBitSize (undefined :: Int) < 64 then [] else
    [(19,4968057848)
    ,(20,39029188884)
    ,(21,314666222712)
    ,(22,2691008701644)
    ,(23,24233937684440)
    ,(24,227514171973736)
    ,(25,2207893435808352)
    ,(26,22317699616364044)
    ]

{-| The maximum board size in 'nqueens_correct_counts'.  In a 64-bit environment
    this value is equal to the largest board size for which we know the number
    of solutions, which is 26.  In a 32-bit environment this value is equal to
    the largest board size such that the number of solutions fits within a
    32-bit (unsigned) integer (i.e., the range of 'Word'), which is 18.
 -}
nqueens_maximum_size :: Int
nqueens_maximum_size = fst . IntMap.findMax $ nqueens_correct_counts

{-| A /partial function/ that returns the number of solutions for the given
    input board size;  this should only be used when you are sure that the input
    is not greater than 'nqueens_maximum_size'.
 -}
nqueensCorrectCount :: Word → Word
nqueensCorrectCount =
    fromJust
    .
    ($ nqueens_correct_counts)
    .
    IntMap.lookup
    .
    fromIntegral

--------------------------------------------------------------------------------
-------------------------------- Basic examples --------------------------------
--------------------------------------------------------------------------------

{- $basic
The two examples in this section are pretty basic in that they do not make use of
the many optimizations that are available (at the cost of code complexity). The first
example uses set operations, and the second uses bitwise operations.
 -}

---------------------------------- Using sets ----------------------------------

{- $sets
The functions in this subsection use 'IntSet's to keep track of which columns
and diagonals are occupied by queens.  (It is not necessarily to keep track of
occupied rows because the rows are filled consecutively.)
 -}

{-| Generate solutions to the n-queens problem using 'IntSet's. -}
nqueensUsingSetsSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensUsingSetsSolutions n =
    go n
       0
       (IntSet.fromDistinctAscList [0..fromIntegral n-1])
       IntSet.empty
       IntSet.empty
       []
  where
    go 0 _ _ _ _ !value = return . reverse $ value
    go !n
       !row
       !available_columns
       !occupied_negative_diagonals
       !occupied_positive_diagonals
       !value
     = do
        column ← allFrom $ IntSet.toList available_columns
        let negative_diagonal = row + column
        guard $ IntSet.notMember negative_diagonal occupied_negative_diagonals
        let positive_diagonal = row - column
        guard $ IntSet.notMember positive_diagonal occupied_positive_diagonals
        go (n-1)
           (row+1)
           (IntSet.delete column available_columns)
           (IntSet.insert negative_diagonal occupied_negative_diagonals)
           (IntSet.insert positive_diagonal occupied_positive_diagonals)
           ((fromIntegral row,fromIntegral column):value)
{-# SPECIALIZE nqueensUsingSetsSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensUsingSetsSolutions :: Word → Tree NQueensSolution #-}
{-# INLINEABLE nqueensUsingSetsSolutions #-}

{-| Generates the solution count to the n-queens problem with the given board
    size;  you need to sum over all these counts to obtain the total, which is
    done by the 'exploreTree' (and related) functions.
 -}
nqueensUsingSetsCount :: MonadPlus m ⇒ Word → m WordSum
nqueensUsingSetsCount = liftM (const $ WordSum 1) . nqueensUsingSetsSolutions
{-# SPECIALIZE nqueensUsingSetsCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensUsingSetsCount :: Word → Tree WordSum #-}
{-# INLINEABLE nqueensUsingSetsCount #-}

---------------------------------- Using bits ----------------------------------

{- $bits
A basic optimization that results in a signiciant performance improvements is to
use 'Word64's as set implemented using bitwise operations --- that is, a bit in
position 1 means that column 1 / negative diagonal 1 / positive diagnal 1 is
occupied.  The total occupied positions can be obtained by taking the bitwise or
of the occupied columns, positive diagonals, and negative diagonals.

Note that when we go to the next row, we shift the negative diagonals right and
the positive diagonals left as every negative/positive diagonal that contains a
square at a given row and column also contains column (x+1)/(x-1) of the
succeeding row.
 -}

{-| Generate solutions to the n-queens problem using bitwise-operations. -}
nqueensUsingBitsSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensUsingBitsSolutions n =
    go n 0 (0::Word64) (0::Word64) (0::Word64) []
  where
    go 0 _ _ _ _ !value = return . reverse $ value
    go !n
       !row
       !occupied_columns
       !occupied_negative_diagonals
       !occupied_positive_diagonals
       !value
     = do
        column ← allFrom . goGetOpenings 0 $
            occupied_columns .|.
            occupied_negative_diagonals .|.
            occupied_positive_diagonals
        let column_bit = bit (fromIntegral column)
        go (n-1)
           (row+1)
           (occupied_columns .|. column_bit)
           ((occupied_negative_diagonals .|. column_bit) `shiftR` 1)
           ((occupied_positive_diagonals .|. column_bit) `shiftL` 1)
           ((row,column):value)

    goGetOpenings column bits
      | column >= n     = []
      | bits .&. 1 == 0 = column:next
      | otherwise       = next
      where
        next = goGetOpenings (column + 1) (bits `shiftR` 1)
{-# SPECIALIZE nqueensUsingBitsSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensUsingBitsSolutions :: Word → Tree NQueensSolution #-}
{-# INLINEABLE nqueensUsingBitsSolutions #-}

{-| Generates the solution count to the n-queens problem with the given board
    size;  you need to sum over all these counts to obtain the total, which is
    done by the 'exploreTree' (and related) functions.
 -}
nqueensUsingBitsCount :: MonadPlus m ⇒ Word → m WordSum
nqueensUsingBitsCount = liftM (const $ WordSum 1) . nqueensUsingBitsSolutions
{-# SPECIALIZE nqueensUsingBitsCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensUsingBitsCount :: Word → Tree WordSum #-}
{-# INLINEABLE nqueensUsingBitsCount #-}

--------------------------------------------------------------------------------
------------------------------- Advanced example -------------------------------
--------------------------------------------------------------------------------

{- $advanced
The advanced example use several techniques to try and squeeze out as much
performance as possible using the functionality of this package. The functions
listed here are just the interface to it; for the implementation driving these
functions, see the "LogicGrowsOnTrees.Examples.Queens.Advanced" module.
 -}

{-| Generates the solutions to the n-queens problem with the given board size. -}
nqueensSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensSolutions n = nqueensGeneric (++) multiplySolution [] n
{-# SPECIALIZE nqueensSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensSolutions :: Word → Tree NQueensSolution #-}

{-| Generates the solution count to the n-queens problem with the given board
    size;  you need to sum over all these counts to obtain the total, which is
    done by the 'exploreTree' (and related) functions.
 -}
nqueensCount :: MonadPlus m ⇒ Word → m WordSum
nqueensCount = nqueensGeneric (const id) (\_ symmetry _ → return . WordSum . Advanced.multiplicityForSymmetry $ symmetry) ()
{-# SPECIALIZE nqueensCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensCount :: Word → Tree WordSum #-}

{-| Like 'nqueensSolutions', but uses List at the bottom instead of C. -}
nqueensWithListAtBottomSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensWithListAtBottomSolutions n = nqueensWithListAtBottomGeneric (++) multiplySolution [] n
{-# SPECIALIZE nqueensWithListAtBottomSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensWithListAtBottomSolutions :: Word → Tree NQueensSolution #-}

{-| Like 'nqueensCount', but uses List at the bottom instead of C. -}
nqueensWithListAtBottomCount :: MonadPlus m ⇒ Word → m WordSum
nqueensWithListAtBottomCount = nqueensWithListAtBottomGeneric (const id) (\_ symmetry _ → return . WordSum . Advanced.multiplicityForSymmetry $ symmetry) ()
{-# SPECIALIZE nqueensWithListAtBottomCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensWithListAtBottomCount :: Word → Tree WordSum #-}

{-| Like 'nqueensSolutions', but uses List at the bottom instead of C. -}
nqueensWithNothingAtBottomSolutions :: MonadPlus m ⇒ Word → m NQueensSolution
nqueensWithNothingAtBottomSolutions n = nqueensWithNothingAtBottomGeneric (++) multiplySolution [] n
{-# SPECIALIZE nqueensWithNothingAtBottomSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensWithNothingAtBottomSolutions :: Word → Tree NQueensSolution #-}

{-| Like 'nqueensCount', but uses List at the bottom instead of C. -}
nqueensWithNothingAtBottomCount :: MonadPlus m ⇒ Word → m WordSum
nqueensWithNothingAtBottomCount = nqueensWithNothingAtBottomGeneric (const id) (\_ symmetry _ → return . WordSum . Advanced.multiplicityForSymmetry $ symmetry) ()
{-# SPECIALIZE nqueensWithNothingAtBottomCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensWithNothingAtBottomCount :: Word → Tree WordSum #-}

