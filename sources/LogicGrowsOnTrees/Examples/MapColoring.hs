{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains examples of 'MonadPlus's that represent the valid
    colorings of a given map.
 -}
module LogicGrowsOnTrees.Examples.MapColoring where

import Control.Monad (MonadPlus,forM_,guard,liftM,when)

import LogicGrowsOnTrees (between)
import LogicGrowsOnTrees.Utils.WordSum

{-| Generate all valid map colorings. -}
coloringSolutions ::
    MonadPlus m ⇒
    Int {-^ the number of colors -} →
    Int {-^ the number of countries -} →
    (Int → Int → Bool) {-^ whether two countries are adjacent -} →
    m [(Int,Int)] {-^ a valid coloring -}
coloringSolutions number_of_colors number_of_countries isAdjacentTo =
    go number_of_countries []
  where
    go 0 coloring = return coloring
    go country coloring = do
        color ← between 1 number_of_colors
        forM_ coloring $ \(other_country, other_color) →
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        go (country-1) ((country,color):coloring)

{-| Generate all valid map colorings. -}
coloringUniqueSolutions ::
    MonadPlus m ⇒
    Int {-^ the number of colors -} →
    Int {-^ the number of countries -} →
    (Int → Int → Bool) {-^ whether two countries are adjacent -} →
    m [(Int,Int)] {-^ a valid coloring -}
coloringUniqueSolutions number_of_colors number_of_countries isAdjacentTo =
    go 0 number_of_countries []
  where
    go _ 0 coloring = return coloring
    go number_of_colors_used country coloring = do
        color ← between 1 ((number_of_colors_used + 1) `min` number_of_colors)
        forM_ coloring $ \(other_country, other_color) →
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        go (number_of_colors_used `max` color) (country-1) ((country,color):coloring)
