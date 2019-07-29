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

import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)
import System.Random (randomRIO)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property (ioProperty)

import LogicGrowsOnTrees.Examples.MapColoring

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests :: [Test]
tests =
    [testGroup "LogicGrowsOnTrees.Examples"
        [testProperty name . ioProperty $ do
            number_of_colors ← randomRIO (2,5)
            number_of_countries ← randomRIO (3,7)
            neighbor_probability ← randomRIO (0,1::Float)
            neighbors ← fmap (concat . concat) $
                forM [1..number_of_countries] $ \x →
                    forM [x+1..number_of_countries] $ \y → do
                        outcome ← randomRIO (0,1)
                        return $
                            if outcome > neighbor_probability
                                then [(x,y),(y,x)]
                                else []
            let solutions =
                    computeSolutions
                        number_of_colors
                        number_of_countries
                        (\x y → (x,y) `elem` neighbors)
            forM_ solutions $ \solution →
                forM_ solution $ \(country_1,color_1) →
                    forM_ solution $ \(country_2,color_2) →
                        when ((country_1,country_2) `elem` neighbors) $
                            assertBool "neighbors have different colors" $ color_1 /= color_2
            let correct_count = sum $ do
                    solution ← zip [1..] <$> replicateM (fromIntegral number_of_countries) [1..number_of_colors]
                    forM_ solution $ \(country_1,color_1) →
                        forM_ solution $ \(country_2,color_2) →
                            when ((country_1,country_2) `elem` neighbors) $
                                guard $ color_1 /= color_2
                    return 1
            computeCount number_of_colors solutions @?= correct_count
            return True
        | (name,computeSolutions,computeCount) ←
            [("coloringSolutions",coloringSolutions,curry (fromIntegral . length . snd))
            ,("coloringUniqueSolutions",coloringUniqueSolutions,
                \number_of_colors →
                    sum
                    .
                    map (\solution →
                        let number_of_colors_used = maximum . fmap snd $ solution
                        in product [number_of_colors-number_of_colors_used+1..number_of_colors]
                    )
             )
            ]
        ]
    ]
