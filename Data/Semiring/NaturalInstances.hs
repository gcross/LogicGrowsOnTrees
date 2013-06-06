-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}  -- this is needed because the compiler assumes that Num Bool might have been defined somewhere
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Data.Semiring.NaturalInstances where

-- Imports {{{
import Data.Composition ((.*))
import Data.Function (on)
import Data.Monoid (Monoid(..))

import Data.Semiring (Semiring(..))
-- }}}

-- Types {{{
newtype N n = N { un :: n } deriving (Eq,Show,Num,Ord)
-- }}}

-- Instances {{{

-- Bool {{{
instance Monoid (N Bool) where
    mempty = N False
    mappend = N .* ((||) `on` un)
instance Semiring (N Bool) where
    munit = N True
    mtimes = N .* ((&&) `on` un)
-- }}}

-- Num {{{
instance Num α ⇒ Monoid (N α) where
    mempty = N 0
    mappend = N .* ((+) `on` un)
instance Num α ⇒ Semiring (N α) where
    munit = N 1
    mtimes = N .* ((*) `on` un)
-- }}}

-- }}}

