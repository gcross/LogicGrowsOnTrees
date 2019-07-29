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

import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Catch (try)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Writer

import qualified Data.Sequence as Seq
import Data.Serialize (encode)

import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Instances ()

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Path

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "LogicGrowsOnTrees.Path"
    [testGroup "sendTreeDownPath"
        [testCase "null path" $ (exploreTree . sendTreeDownPath Seq.empty) (return [42::Int]) @?= [42]
        ,testCase "cache" $ do (exploreTree . sendTreeDownPath (Seq.singleton (CacheStep (encode ([42 :: Int]))))) (cache (undefined :: [Int])) @?= [42]
        ,testCase "cacheGuard" $ do (exploreTree . sendTreeDownPath (Seq.singleton (CacheStep (encode ())))) (cacheGuard False >> return [42::Int]) @?= [42]
        ,testCase "choice" $ do
            (exploreTree . sendTreeDownPath (Seq.singleton (ChoiceStep LeftBranch))) (return [42::Int] `mplus` undefined) @?= [42]
            (exploreTree . sendTreeDownPath (Seq.singleton (ChoiceStep RightBranch))) (undefined `mplus` return [42::Int]) @?= [42]
        ,testGroup "errors"
            [testGroup "PastTreeIsInconsistentWithPresentTree"
                [testCase "cache step with choice" $
                    try (
                        evaluate
                        .
                        exploreTree
                        $
                        sendTreeDownPath (Seq.singleton (CacheStep undefined :: Step)) (undefined `mplus` undefined :: Tree [Int])
                    ) >>= (@?= Left PastTreeIsInconsistentWithPresentTree)
                ,testCase "choice step with cache" $
                    try (
                        evaluate
                        .
                        exploreTree
                        $
                        sendTreeDownPath (Seq.singleton (ChoiceStep undefined :: Step)) (cache undefined :: Tree [Int])
                    ) >>= (@?= Left PastTreeIsInconsistentWithPresentTree)
                ]
            ,testGroup "TreeEndedBeforeEndOfWalk"
                [testCase "mzero" $
                    try (
                        evaluate
                        .
                        exploreTree
                        $
                        sendTreeDownPath (Seq.singleton (undefined :: Step)) (mzero :: Tree [Int])
                    ) >>= (@?= Left TreeEndedBeforeEndOfWalk)
                ,testCase "return" $
                    try (
                        evaluate
                        .
                        exploreTree
                        $
                        sendTreeDownPath (Seq.singleton (undefined :: Step)) (return (undefined :: [Int]))
                    ) >>= (@?= Left TreeEndedBeforeEndOfWalk)
                ]
            ]
        ]
    ,testGroup "sendTreeTDownPath"
        [testCase "cache step" $ do
            let (transformed_tree,logged) =
                    runWriter . sendTreeTDownPath (Seq.singleton (CacheStep . encode $ [24 :: Int])) $ do
                        runAndCache (tell [1] >> return [42 :: Int] :: Writer [Int] [Int])
            logged @?= []
            (runWriter . exploreTreeT $ transformed_tree) @?= ([24],[])
        ,testCase "choice step" $ do
            let (transformed_tree,logged) =
                    runWriter . sendTreeTDownPath (Seq.singleton (ChoiceStep RightBranch)) $ do
                        lift (tell [1::Int])
                        (lift (tell [2]) `mplus` lift (tell [3]))
                        lift (tell [4])
                        return [42::Int]
            logged @?= [1]
            (runWriter . exploreTreeT $ transformed_tree) @?= ([42],[3,4])
        ]
    ]
