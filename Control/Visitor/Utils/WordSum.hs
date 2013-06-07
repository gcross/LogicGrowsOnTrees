-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Utils.WordSum where

-- Imports {{{
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Serialize (Serialize(..))
import Data.Typeable (Typeable)
import Data.Word (Word)
-- }}}

-- Types {{{
data WordSum = WordSum { getWordSum :: {-# UNPACK #-} !Word } deriving (Eq,Show,Typeable)
-- }}}

-- Instances {{{

instance Monoid WordSum where -- {{{
    mempty = WordSum 0
    WordSum x `mappend` WordSum y = WordSum (x+y)
    mconcat = foldl' mappend mempty
-- }}}

instance Serialize WordSum where -- {{{
    put = put . getWordSum
    get = fmap WordSum get
-- }}}

-- }}}
