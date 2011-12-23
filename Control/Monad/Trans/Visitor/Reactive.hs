-- @+leo-ver=5-thin
-- @+node:gcross.20111223134617.1431: * @file Reactive.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111223134617.1432: ** << Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Reactive where

-- @+<< Import needed modules >>
-- @+node:gcross.20111223134617.1433: ** << Import needed modules >>
import Control.Arrow ((&&&))
import Control.Monad.IO.Class

import Data.Either.Unwrap (fromLeft,fromRight,isLeft,isRight)
import Data.Maybe (fromJust,isJust)

import Reactive.Banana (fromAddHandler,newAddHandler)
import Reactive.Banana.Model
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111223134617.1434: ** Functions
-- @+node:gcross.20111219132352.1428: *3* (<$?>)
infixl 4 <$?>

(<$?>) :: FRP ξ ⇒ (a → Maybe b) → Event ξ a → Event ξ b
f <$?> x = filterJust (f <$> x)
-- @+node:gcross.20111223170956.1440: *3* (<$?↔>)
infixl 4 <$?↔>

(<$?↔>) :: FRP ξ ⇒ (a → Maybe (Either b c)) → Event ξ a → (Event ξ b,Event ξ c)
f <$?↔> x = split . filterJust $ (f <$> x)
-- @+node:gcross.20111219132352.1427: *3* (<$↔>)
infixl 4 <$↔>

(<$↔>) :: FRP ξ ⇒ (a → Either b c) → Event ξ a → (Event ξ b,Event ξ c)
f <$↔> x = split (f <$> x)
-- @+node:gcross.20111223134617.1427: *3* (<@?>)
infixl 4 <@?>

(<@?>) :: (FRP ξ, Apply f (Event ξ)) ⇒ f (a → Maybe b) → Event ξ a → Event ξ b
f <@?> x = filterJust (f <@> x)
-- @+node:gcross.20111026220221.1283: *3* (<@↔>)
infixl 4 <@↔>

(<@↔>) :: (FRP ξ, Apply f (Event ξ)) ⇒ f (a → Either b c) → Event ξ a → (Event ξ b,Event ξ c)
f <@↔> x = split (f <@> x)
-- @+node:gcross.20111026213013.1279: *3* filterJust
filterJust :: FRP ξ ⇒ Event ξ (Maybe a) → Event ξ a
filterJust = fmap fromJust . filterE isJust
-- @+node:gcross.20111026213013.1281: *3* newHandler
newHandler = do
    (event_handler,callback) ← liftIO newAddHandler
    event ← fromAddHandler event_handler
    return (event,callback)
-- @+node:gcross.20111026213013.1282: *3* split
split :: FRP ξ ⇒ Event ξ (Either a b) → (Event ξ a,Event ξ b)
split = (fmap fromLeft . filterE isLeft) &&& (fmap fromRight . filterE isRight)
-- @-others
-- @-leo
