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

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Writer

import Data.Functor.Identity
import qualified Data.IntSet as IntSet

import System.IO.Unsafe
import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen hiding (shuffle)
import Test.QuickCheck.Instances ()

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Testing

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "LogicGrowsOnTrees"
    [testGroup "Eq instance"
        [testProperty "self" $ \(v :: Tree [()]) → v == v
        ]
    ,testProperty "allFrom" $ \(x :: [Int]) → x == allFrom x
    ,testProperty "between" $ do
        x ← choose ( 0,100) :: Gen Int
        y ← choose (50,100)
        return $ between x y == [x..y]
    ,testGroup "exploreTree"
        [testCase "return" $ exploreTree (return [()]) @?= [()]
        ,testCase "mzero" $ exploreTree (mzero :: Tree [()]) @?= []
        ,testCase "mplus" $ exploreTree (return [1::Int] `mplus` return [2]) @?= [1,2]
        ,testCase "cache" $ exploreTree (cache [42]) @?= [42::Int]
        ,testGroup "cacheMaybe"
            [testCase "Nothing" $ exploreTree (cacheMaybe (Nothing :: Maybe [()])) @?= []
            ,testCase "Just" $ exploreTree (cacheMaybe (Just [42])) @?= [42::Int]
            ]
        ,testGroup "cacheGuard"
            [testCase "True" $ exploreTree (cacheGuard False >> return [()]) @?= []
            ,testCase "False" $ exploreTree (cacheGuard True >> return [()]) @?= [()]
            ]
        ]
    ,testGroup "exploreTreeT"
        [testCase "Writer" $
            (runWriter . exploreTreeT $ do
                cache [1 :: Int] >>= lift . tell
                (lift (tell [2]) `mplus` lift (tell [3]))
                return [42::Int]
            ) @?= ([42,42],[1,2,3])
        ]
    ,testGroup "exploreTreeTAndIgnoreResults"
        [testCase "Writer" $
            (runWriter . exploreTreeTAndIgnoreResults $ do
                cache [1 :: Int] >>= lift . tell
                (lift (tell [2]) `mplus` lift (tell [3]))
                return [42::Int]
            ) @?= ((),[1,2,3])
        ]
    ,testGroup "exploreTreeUntilFirst"
        [testCase "return" $ exploreTreeUntilFirst (return 42) @=? (Just 42 :: Maybe Int)
        ,testCase "null" $ exploreTreeUntilFirst mzero @=? (Nothing :: Maybe Int)
        ,testProperty "compared to exploreTree" $ \(tree :: Tree String) →
            exploreTreeUntilFirst tree
            ==
            case exploreTree (fmap (:[]) tree) of
                [] → Nothing
                (x:_) → Just x
        ]
    ,testGroup "exploreTreeTUntilFirst"
        [testCase "return" $ runIdentity (exploreTreeTUntilFirst (return 42)) @=? (Just 42 :: Maybe Int)
        ,testCase "null" $ runIdentity(exploreTreeTUntilFirst mzero) @=? (Nothing :: Maybe Int)
        ,testProperty "compared to exploreTreeT" $ \(tree :: TreeT Identity String) →
            runIdentity (exploreTreeTUntilFirst tree)
            ==
            case runIdentity (exploreTreeT (fmap (:[]) tree)) of
                [] → Nothing
                (x:_) → Just x
        ]
    ,testGroup "exploreTreeUntilFound"
        [testProperty "compared to exploreTree" $ do
            UniqueTree tree ← arbitrary
            let solutions = exploreTree tree
            threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
            pure . unsafePerformIO . checkFoundAgainstThreshold threshold solutions $
                exploreTreeUntilFound ((>= threshold) . IntSet.size) tree
        ]
    ,testGroup "exploreTreeTUntilFound"
        [testProperty "compared to exploreTreeT" $ do
            UniqueTree tree ← arbitrary
            let solutions = runIdentity (exploreTreeT tree)
            threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
            return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions . runIdentity $
                exploreTreeTUntilFound ((>= threshold) . IntSet.size) tree
        ]
    ]
