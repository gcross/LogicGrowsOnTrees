-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Data.Semiring where

-- Imports {{{
import Control.Monad (liftM2)

import Data.Composition ((.*))
import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import Data.Function (on)
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
-- }}}

-- Classes {{{

class Monoid α ⇒ Semiring α where -- {{{
    munit :: α
    mtimes :: α → α → α
    mproduct :: Foldable t ⇒ t α → α
    mproduct = Foldable.foldr mtimes munit
infix 7 `mtimes`
-- }}}

-- }}}

-- Instances {{{

instance Semiring All where -- {{{
    munit = All False
    mtimes = All .* ((||) `on` getAll)
-- }}}

instance Semiring Any where -- {{{
    munit = Any True
    mtimes = Any .* ((&&) `on` getAny)
-- }}}

instance Num α ⇒ Semiring (Sum α) where -- {{{
    munit = Sum 1
    mtimes = Sum .* ((*) `on` getSum)
    mproduct = Foldable.foldl' mtimes munit
-- }}}

-- }}}
