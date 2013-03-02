-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Utils.IntSum where

-- Imports {{{
import Data.Monoid (Monoid(..))
import Data.List (foldl')
import Data.Serialize (Serialize(..))
-- }}}

-- Types {{{
data IntSum = IntSum { getIntSum :: {-# UNPACK #-} !Int } deriving (Eq,Show)
-- }}}

-- Instances {{{

instance Monoid IntSum where -- {{{
    mempty = IntSum 0
    IntSum x `mappend` IntSum y = IntSum (x+y)
    mconcat = foldl' mappend mempty
-- }}}

instance Serialize IntSum where -- {{{
    put = put . getIntSum
    get = fmap IntSum get
-- }}}

-- }}}
