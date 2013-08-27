{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains examples of 'MonadPlus's that represent the valid
    colorings of a given map.
 -}
module LogicGrowsOnTrees.Examples.MapColoring where

import Control.Monad (MonadPlus,foldM,forM_,guard,liftM,when)
import Data.Word (Word)

import LogicGrowsOnTrees (between)

{-| Generate all valid map colorings. -}
coloringSolutions ::
    MonadPlus m ⇒
    Word {-^ the number of colors -} →
    Word {-^ the number of countries -} →
    (Word → Word → Bool) {-^ whether two countries are adjacent -} →
    m [(Word,Word)] {-^ a valid coloring -}
coloringSolutions number_of_colors number_of_countries isAdjacentTo =
    foldM addCountryToColoring [] [1..number_of_countries]
  where
    addCountryToColoring coloring country = do
        color ← between 1 number_of_colors
        forM_ coloring $ \(other_country, other_color) →
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        return $ (country,color):coloring

{-| Generate all valid map colorings. -}
coloringUniqueSolutions ::
    MonadPlus m ⇒
    Word {-^ the number of colors -} →
    Word {-^ the number of countries -} →
    (Word → Word → Bool) {-^ whether two countries are adjacent -} →
    m [(Word,Word)] {-^ a valid coloring -}
coloringUniqueSolutions number_of_colors number_of_countries isAdjacentTo =
    liftM snd $ foldM addCountryToColoring (0,[]) [1..number_of_countries]
  where
    addCountryToColoring (number_of_colors_used,coloring) country = do
        color ← between 1 ((number_of_colors_used + 1) `min` number_of_colors)
        forM_ coloring $ \(other_country, other_color) →
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        return (number_of_colors_used `max` color,(country,color):coloring)
