{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains examples of 'MonadPlus's that represent the valid
    colorings of a given map.
 -}
module LogicGrowsOnTrees.Examples.MapColoring where

import Control.Monad (MonadPlus,forM_,guard,when)
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
    Word {-^ the number of colors -} →
    Word {-^ the number of countries -} →
    (Word → Word → Bool) {-^ whether two countries are adjacent -} →
    m [(Word,Word)] {-^ a valid coloring -}
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
