{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains types that represent the purity of a Tree.  Trees are
    either pure or impure running on top of IO, with the value io_purity
    being a special case of the latter.
 -}
module LogicGrowsOnTrees.Parallel.Common.Purity
    ( Purity(..)
    , io_purity
    ) where

import Control.Monad.IO.Class

import Data.Functor.Identity (Identity)

{-| The purity of a tree;  the options are 'Pure' for pure trees
    and 'ImpureAtopIO' for impure trees, where the latter case is restricted to
    monads that are instances of 'MonadIO' and provide a way to convert the
    monad into an IO action.

    The two kind argument, m and n, correspond to respectively the monad in
    on top of which the TreeT monad transformer is stacked and the monad in
    which the worker will be run.
 -} 
data Purity (m :: * → *) (n :: * → *) where -- {{{
    Pure :: Purity Identity IO
    ImpureAtopIO :: MonadIO m ⇒ (∀ β. m β → IO β) → Purity m m

{-| The purity of trees in the IO monad. -}
io_purity = ImpureAtopIO id
