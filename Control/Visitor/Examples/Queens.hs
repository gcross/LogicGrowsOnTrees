-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Examples.Queens where

-- Imports {{{
import Control.Monad (MonadPlus)

import Data.Bits (bitSize)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)
import Data.Word (Word)

import System.Console.CmdTheLine (ArgVal(..),just)

import Text.PrettyPrint (text)

import Control.Visitor (Visitor)
import Control.Visitor.Examples.Queens.Implementation
import Control.Visitor.Utils.WordSum
-- }}}

-- Types {{{
newtype BoardSize = BoardSize { getBoardSize :: Int }
instance ArgVal BoardSize where -- {{{
    converter = (parseBoardSize,prettyBoardSize)
      where
        (parseInt,prettyInt) = converter
        parseBoardSize =
            either Left (\n →
                if n >= 1 && n <= nqueens_maximum_size
                    then Right . BoardSize $ n
                    else Left . text $ "bad board size (must be between 1 and " ++ show nqueens_maximum_size ++ " inclusive)"
            )
            .
            parseInt
        prettyBoardSize = prettyInt . getBoardSize
instance ArgVal (Maybe BoardSize) where
    converter = just
-- }}}
-- }}}

-- Values -- {{{

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
    ] ++ if bitSize (undefined :: Int) < 64 then [] else
    [(19,4968057848)
    ,(20,39029188884)
    ,(21,314666222712)
    ,(22,2691008701644)
    ,(23,24233937684440)
    ,(24,227514171973736)
    ,(25,2207893435808352)
    ,(26,22317699616364044)
    ]

nqueens_maximum_size = fst . IntMap.findMax $ nqueens_correct_counts

-- }}}

-- Functions {{{

nqueensCorrectCount :: Word → Word -- {{{
nqueensCorrectCount =
    fromJust
    .
    ($ nqueens_correct_counts)
    .
    IntMap.lookup
    .
    fromIntegral
-- }}}

nqueensCount :: MonadPlus m ⇒ Word → m WordSum -- {{{
nqueensCount = nqueensGeneric (const id) (\_ symmetry _ → return . WordSum . multiplicityForSymmetry $ symmetry) ()
{-# SPECIALIZE nqueensCount :: Word → [WordSum] #-}
{-# SPECIALIZE nqueensCount :: Word → Visitor WordSum #-}
-- }}}

nqueensSolutions :: MonadPlus m ⇒ Word → m NQueensSolution -- {{{
nqueensSolutions n = nqueensGeneric (++) multiplySolution [] n
{-# SPECIALIZE nqueensSolutions :: Word → NQueensSolutions #-}
{-# SPECIALIZE nqueensSolutions :: Word → Visitor NQueensSolution #-}
-- }}}

-- }}}
