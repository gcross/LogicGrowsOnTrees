{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

import qualified Data.IntSet as IntSet

import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Instances ()

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Testing

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "test helpers"
    [testProperty "UniqueTree has unique results" $ \(UniqueTree tree) →
        let results = exploreTree (fmap (:[]) tree )
        in length results == IntSet.size (mconcat results)
    ]
