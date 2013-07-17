{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains a type that specializes the 'Sum' 'Monoid' to 'Word'. -}
module LogicGrowsOnTrees.Utils.WordSum where

import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Serialize (Serialize(..))
import Data.Typeable (Typeable)
import Data.Word (Word)

{-| A datatype that contains an unboxed 'Word'. -}
data WordSum = WordSum { getWordSum :: {-# UNPACK #-} !Word } deriving (Eq,Show,Typeable)

{-| This instance sums the two contained 'Word's. -}
instance Monoid WordSum where
    mempty = WordSum 0
    WordSum x `mappend` WordSum y = WordSum (x+y)
    mconcat = foldl' mappend mempty

{-| This instances is equivalent to the instance for 'Word'. -}
instance Serialize WordSum where
    put = put . getWordSum
    get = fmap WordSum get



