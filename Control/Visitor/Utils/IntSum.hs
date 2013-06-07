-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Utils.IntSum where

-- Imports {{{
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Serialize (Serialize(..))
import Data.Typeable (Typeable)
-- }}}

-- Types {{{
data IntSum = IntSum { getIntSum :: {-# UNPACK #-} !Int } deriving (Eq,Show,Typeable)
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
