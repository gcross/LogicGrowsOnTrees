{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Control.Exception (bracket)

import Data.UUID (UUID)

import System.Directory (getTemporaryDirectory,removeFile)
import System.IO
import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import qualified Test.Framework.Providers.SmallCheck as Small
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property (ioProperty)
import Test.SmallCheck ((==>))
import Test.SmallCheck.Drivers as Small (test)

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Testing ()
import LogicGrowsOnTrees.Utils.Handle (send,receive)
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests :: [Test]
tests =
    [testProperty "LogicGrowsOnTrees.Utils.Handle" $ \(x::UUID) → ioProperty $
        bracket
            (getTemporaryDirectory >>= flip openBinaryTempFile "test-handles")
            (\(filepath,handle) → do
                hClose handle
                removeFile filepath
            )
            (\(_,handle) → do
                send handle x
                hSeek handle AbsoluteSeek 0
                y ← receive handle
                return (x == y)
            )
    ,testGroup "LogicGrowsOnTrees.Utils.PerfectTree"
        [Small.testProperty "trivialPerfectTree" . Small.test $
            (liftA2 . liftA2) (==>)
                (\arity _ → arity >= 2)
                ((liftA2 . liftA2) (==)
                    numberOfLeaves
                    (\arity depth → getWordSum . exploreTree $ trivialPerfectTree arity depth)
                )
        ]
    ]
