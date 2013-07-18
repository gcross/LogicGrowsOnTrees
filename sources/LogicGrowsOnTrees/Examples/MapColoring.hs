{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains examples of 'MonadPlus's that represent the valid
    colorings of a given map.
 -}
module LogicGrowsOnTrees.Examples.MapColoring where

import Control.Monad (MonadPlus,forM_,guard,liftM,when)

import LogicGrowsOnTrees (allFrom)
import LogicGrowsOnTrees.Utils.WordSum

{-| Generate all valid colorings for the given colors, countries, and neighbor relation. -}
coloringSolutions ::
    ( Eq color
    , Eq country
    , MonadPlus m
    ) ⇒
    [color] {-^ the available colors -} →
    [country] {-^ the countries to color -} →
    (country → country → Bool) {-^ a (symmetric) function that returns 'True' if two countries are neighbors and 'False' otherwise -} →
    m [(country,color)] {-^ a valid coloring -}
coloringSolutions colors countries isNeighborOf = go countries []
  where
    go [] coloring = return . reverse $ coloring
    go (country:rest_countries) coloring = do
        color ← allFrom colors
        forM_ coloring $ \(other_country, other_color) →
            when (country `isNeighborOf` other_country) $
                guard (color /= other_color)
        go rest_countries ((country,color):coloring)

{-^ Generates the number of possible colorings for the given colors, countries, and neighbor relation. -}
coloringCount ::
    ( Eq color
    , Eq country
    , MonadPlus m
    ) ⇒
    [color] {-^ the available colors -} →
    [country] {-^ the countries to color -} →
    (country → country → Bool) {-^ a (symmetric) function that returns 'True' if two countries are neighbors and 'False' otherwise -} →
    m WordSum
coloringCount colors countries isNeighborOf =
    liftM (const $ WordSum 1) (coloringSolutions colors countries isNeighborOf)