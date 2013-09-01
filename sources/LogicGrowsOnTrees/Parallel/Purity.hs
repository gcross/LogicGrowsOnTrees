{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains types that represent the purity of a tree, which is
    either pure, impure, or IO (a special case of impure).
 -}
module LogicGrowsOnTrees.Parallel.Purity
    ( Purity(..)
    , io_purity
    ) where

import Control.Monad.IO.Class

import Data.Functor.Identity (Identity)

{-| The purity of a tree, which can be either 'Pure' (for pure trees) and
    'ImpureAtopIO' (for impure trees); the latter case is restricted to monads
    that are instances of 'MonadIO' and for which there exists a way to convert
    the monad into an IO action.

    The two kind arguments, @m@ and @n@, correspond to respectively the monad in
    on top of which the 'TreeT' monad transformer is stacked and the monad in
    which the worker will be run.
 -} 
data Purity (m :: * → *) (n :: * → *) where -- {{{
    Pure :: Purity Identity IO
    ImpureAtopIO :: MonadIO m ⇒ (∀ β. m β → IO β) → Purity m m

{-| The purity of trees in the IO monad. -}
io_purity :: Purity IO IO
io_purity = ImpureAtopIO id
