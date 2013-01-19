-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Examples.Queens
    ( IntSum
    , NQueensSolution
    , NQueensSolutions
    , nqueens_correct_counts
    , nqueensCorrectCount
    , nqueensCount
    , nqueensSolutions
    ) where

-- Imports {{{
import Control.Monad (MonadPlus)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)

import Control.Monad.Trans.Visitor (IntSum(IntSum),Visitor)
import Control.Monad.Trans.Visitor.Examples.Queens.Implementation
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

nqueensCorrectCount :: Int → Int -- {{{
nqueensCorrectCount = fromJust . ($ nqueens_correct_counts) . IntMap.lookup
-- }}}

nqueensCount :: MonadPlus m ⇒ Int → m IntSum -- {{{
nqueensCount n = nqueensGeneric n
{-# SPECIALIZE nqueensCount :: Int → [IntSum] #-}
{-# SPECIALIZE nqueensCount :: Int → Visitor IntSum #-}
-- }}}

nqueensSolutions :: MonadPlus m ⇒ Int → m NQueensSolution -- {{{
nqueensSolutions n = nqueensGeneric n
{-# SPECIALIZE nqueensSolutions :: Int → NQueensSolutions #-}
{-# SPECIALIZE nqueensSolutions :: Int → Visitor NQueensSolution #-}
-- }}}

-- }}}