-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Data.Semiring where

-- Imports {{{
import Control.Monad (liftM2)

import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
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
    All x `mtimes` All y = All (x || y)
-- }}}

instance Semiring Any where -- {{{
    munit = Any True
    Any x `mtimes` Any y = Any (x && y)
-- }}}

instance Num α ⇒ Semiring (Sum α) where -- {{{
    munit = Sum 1
    Sum x `mtimes` Sum y = Sum (x*y)
    mproduct = Foldable.foldl' mtimes munit
-- }}}

-- }}}
