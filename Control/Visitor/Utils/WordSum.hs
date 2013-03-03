-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Utils.WordSum where

-- Imports {{{
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Semiring (Semiring(..))
import Data.Serialize (Serialize(..))
import Data.Word (Word)
-- }}}

-- Types {{{
data WordSum = WordSum { getWordSum :: {-# UNPACK #-} !Word } deriving (Eq,Show)
-- }}}

-- Instances {{{

instance Monoid WordSum where -- {{{
    mempty = WordSum 0
    WordSum x `mappend` WordSum y = WordSum (x+y)
    mconcat = foldl' mappend mempty
-- }}}

instance Semiring WordSum where -- {{{
    munit = WordSum 1
    WordSum x `mtimes` WordSum y = WordSum (x*y)
-- }}}

instance Serialize WordSum where -- {{{
    put = put . getWordSum
    get = fmap WordSum get
-- }}}

-- }}}
