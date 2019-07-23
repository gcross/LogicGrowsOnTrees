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
import Control.Monad.Catch (try)

import Data.Functor.Identity
import qualified Data.IntSet as IntSet
import Data.Serialize (encode)
import Data.Set (Set)
import Data.UUID (UUID)

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
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Testing

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "LogicGrowsOnTrees.Checkpoint"
    [testGroup "contextFromCheckpoint"
        [testProperty "cache" $ \(checkpoint :: Checkpoint) (i :: Int) →
            checkpointFromContext [CacheContextStep (encode i)] checkpoint
            ==
            (simplifyCheckpointRoot $ CachePoint (encode i) checkpoint)
        ,testProperty "left branch" $ \(inner_checkpoint :: Checkpoint) (other_tree :: Tree [()]) (other_checkpoint :: Checkpoint) →
            (checkpointFromContext [LeftBranchContextStep other_checkpoint other_tree] inner_checkpoint)
            ==
            (simplifyCheckpointRoot $ ChoicePoint inner_checkpoint other_checkpoint)
        ,testProperty "right branch" $ \(checkpoint :: Checkpoint) →
            checkpointFromContext [RightBranchContextStep] checkpoint
            ==
            (simplifyCheckpointRoot $ ChoicePoint Explored checkpoint)
        ,testProperty "empty" $ \(checkpoint :: Checkpoint) →
            checkpointFromContext [] checkpoint == checkpoint
        ]
    ,testProperty "invertCheckpoint" $ \(tree :: Tree (Set UUID)) →
        randomCheckpointForTree tree >>= \(partial_result,checkpoint) → return $
            partial_result == exploreTreeStartingFromCheckpoint (invertCheckpoint checkpoint) tree
    ,testGroup "Monoid instance"
        [testProperty "product results in intersection of solutions" $ \(UniqueTree tree) → do
            (_,checkpoint1) ← randomCheckpointForTree tree
            (_,checkpoint2) ← randomCheckpointForTree tree
            let checkpoint3 = checkpoint1 `mappend` checkpoint2
                solutions1 = exploreTreeStartingFromCheckpoint checkpoint1 tree
                solutions2 = exploreTreeStartingFromCheckpoint checkpoint2 tree
                solutions3 = exploreTreeStartingFromCheckpoint checkpoint3 tree
            return $ solutions3 == solutions1 `IntSet.intersection` solutions2
        ,testCase "throws the correct exceptions" $
            mapM_ (\(x,y) →
                try (
                    evaluate (x `mappend` y)
                    >>
                    assertFailure (show x ++ " and " ++ show y ++ " were not recognized as being inconsistent")
                )
                >>=
                either
                    (assertEqual "the thrown exception was incorrect" (InconsistentCheckpoints x y))
                    (const $ assertFailure "exception was not thrown")
            )
            [((CachePoint (encode (42 :: Int)) Unexplored),(CachePoint (encode (42 :: Integer)) Unexplored))
            ,((ChoicePoint Unexplored Unexplored),CachePoint (encode (42 :: Int)) Unexplored)
            ]
        ,testProperty "unit element laws" $ \(checkpoint :: Checkpoint) →
            (mempty `mappend` checkpoint == checkpoint) && (checkpoint `mappend` mempty == checkpoint)
        ]
    ,testProperty "stepThroughTreeStartingFromCheckpoint" $ do
        UniqueTree tree ← arbitrary
        (partial_result,checkpoint) ← randomCheckpointForTree tree
        let go state@ExplorationTState{..} current_result =
                exploreTreeStartingFromCheckpoint
                    (invertCheckpoint (checkpointFromExplorationState state))
                    tree
                ==
                current_result
                &&
                case stepThroughTreeStartingFromCheckpoint state of
                    (Just result,Nothing) → exploreTree tree == current_result <> result
                    (Nothing,Nothing) → exploreTree tree == current_result
                    (Just result,Just new_state) → go new_state (current_result <> result)
                    (Nothing,Just new_state) → go new_state current_result
        return $ go (initialExplorationState checkpoint tree) partial_result
    ,testProperty "exploreTreeStartingFromCheckpoint" $ \(UniqueTree tree) →
        randomCheckpointForTree tree >>= \(partial_result,checkpoint) → return $
            exploreTree tree ==
                mappend partial_result (exploreTreeStartingFromCheckpoint checkpoint tree)
    ,testProperty "exploreTreeUntilFirstStartingFromCheckpoint" $ \(UniqueTree tree) →
        randomCheckpointForTree tree >>= \(_,checkpoint) → return $
            let all_results = exploreTreeStartingFromCheckpoint checkpoint tree
                maybe_first_result = exploreTreeUntilFirstStartingFromCheckpoint checkpoint tree
            in case maybe_first_result of
                Nothing → IntSet.null all_results
                Just result → IntSet.size result == 1 && IntSet.member (IntSet.findMin result) all_results
    ,testProperty "exploreTreeTUntilFirstStartingFromCheckpoint" $ \(UniqueTree tree) →
        randomCheckpointForTree tree >>= \(_,checkpoint) → return $
            let all_results = exploreTreeStartingFromCheckpoint checkpoint tree
                maybe_first_result = runIdentity $ exploreTreeTUntilFirstStartingFromCheckpoint checkpoint tree
            in case maybe_first_result of
                Nothing → IntSet.null all_results
                Just result → IntSet.size result == 1 && IntSet.member (IntSet.findMin result) all_results
    ,testProperty "exploreTreeUntilFoundStartingFromCheckpoint" $ do
        UniqueTree tree ← arbitrary
        (_,checkpoint) ← randomCheckpointForTree tree
        let solutions = exploreTreeStartingFromCheckpoint checkpoint tree
        threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
        return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions $
            exploreTreeUntilFoundStartingFromCheckpoint ((>= threshold) . IntSet.size) checkpoint tree
    ,testProperty "exploreTreeTUntilFoundStartingFromCheckpoint" $ do
        UniqueTree tree ← arbitrary
        (_,checkpoint) ← randomCheckpointForTree tree
        let solutions = exploreTreeStartingFromCheckpoint checkpoint tree
        threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
        return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions . runIdentity $
            exploreTreeTUntilFoundStartingFromCheckpoint ((>= threshold) . IntSet.size) checkpoint tree
    ]
