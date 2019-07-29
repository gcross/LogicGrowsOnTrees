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
import Control.Monad

import Data.Function
import Data.Monoid

import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Gen hiding (shuffle)
import Test.QuickCheck.Instances ()

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Location
import LogicGrowsOnTrees.Path
import LogicGrowsOnTrees.Testing

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests :: [Test]
tests =
    [testGroup "LogicGrowsOnTrees.Location"
        [testProperty "branchingFromLocation . labelFromBranching = id" $
            liftA2 (==)
                (branchingFromLocation . labelFromBranching)
                id
        ,testProperty "labelFromBranching . branchingFromLocation = id" $
            liftA2 (==)
                (labelFromBranching . branchingFromLocation)
                id
        ,testGroup "Monoid instance"
            [testProperty "equivalent to concatenation of branchings" $ \(parent_branching :: [BranchChoice]) (child_branching :: [BranchChoice]) →
                labelFromBranching parent_branching `mappend` labelFromBranching child_branching
                ==
                labelFromBranching (parent_branching `mappend` child_branching)
            ,testProperty "obeys monoid laws" $
                liftA2 (&&)
                    (liftA2 (==) id (`mappend` (mempty :: Location)))
                    (liftA2 (==) id ((mempty :: Location) `mappend`))
            ]
        ,testProperty "Ord instance of Location equivalent to Ord of branching" $ \a b →
            (compare `on` branchingFromLocation) a b == compare a b
        ,testGroup "exploreTreeWithLocations"
            [testProperty "same result as exploreTree" $ \(tree :: Tree [()]) →
                 exploreTree ((:[]) <$> tree) == (solutionResult <$> exploreTreeWithLocations tree)
            ]
        ,testGroup "sendTreeDownLocation"
            [testProperty "same result as walking down path" $ do
                tree :: Tree Int ← randomTreeWithoutCache
                path ← randomPathForTree tree
                let label = labelFromPath path
                return $
                    sendTreeDownPath path tree
                    ==
                    sendTreeDownLocation label tree
            ]
        ,testProperty "exploreLocatableTree" $
            let gen _ 0 = return mzero
                gen label 1 = return (All . (== label) <$> getLocation)
                gen label n = do
                    left_size ← choose (0,n)
                    let right_size = n-left_size
                    left ← gen (leftBranchOf label) left_size
                    right ← gen (rightBranchOf label) right_size
                    return $ left `mplus` right
            in getAll . exploreLocatableTree <$> sized (gen rootLocation)
        ]
    ]
