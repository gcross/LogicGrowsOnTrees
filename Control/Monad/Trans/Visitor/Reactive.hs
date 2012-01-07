-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Reactive where

-- Imports {{{
import Control.Arrow ((&&&))
import Control.Monad.IO.Class

import Data.Either.Unwrap (fromLeft,fromRight,isLeft,isRight)
import Data.Maybe (fromJust,isJust)

import Reactive.Banana (fromAddHandler,newAddHandler)
import Reactive.Banana.Model
-- }}}

-- Functions/Operators {{{

(<$?>) :: FRP ξ ⇒ (a → Maybe b) → Event ξ a → Event ξ b -- {{{
f <$?> x = filterJust (f <$> x)
infixl 4 <$?>
-- }}}

(<$?↔>) :: FRP ξ ⇒ (a → Maybe (Either b c)) → Event ξ a → (Event ξ b,Event ξ c) -- {{{
f <$?↔> x = split . filterJust $ (f <$> x)
infixl 4 <$?↔>
-- }}}

(<$↔>) :: FRP ξ ⇒ (a → Either b c) → Event ξ a → (Event ξ b,Event ξ c) -- {{{
f <$↔> x = split (f <$> x)
infixl 4 <$↔>
-- }}}

(<@?>) :: (FRP ξ, Apply f (Event ξ)) ⇒ f (a → Maybe b) → Event ξ a → Event ξ b -- {{{
f <@?> x = filterJust (f <@> x)
infixl 4 <@?>
-- }}}

(<@↔>) :: (FRP ξ, Apply f (Event ξ)) ⇒ f (a → Either b c) → Event ξ a → (Event ξ b,Event ξ c) -- {{{
f <@↔> x = split (f <@> x)
infixl 4 <@↔>
-- }}}

newHandler = do -- {{{
    (event_handler,callback) ← liftIO newAddHandler
    event ← fromAddHandler event_handler
    return (event,callback)
-- }}}

split :: FRP ξ ⇒ Event ξ (Either a b) → (Event ξ a,Event ξ b) -- {{{
split = (fmap fromLeft . filterE isLeft) &&& (fmap fromRight . filterE isRight)
-- }}}

-- }}}
