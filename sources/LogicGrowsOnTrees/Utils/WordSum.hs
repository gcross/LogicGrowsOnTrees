{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains a type that specializes the 'Sum' 'Monoid' to 'Word'. -}
module LogicGrowsOnTrees.Utils.WordSum where

import Control.DeepSeq (NFData(..))
import Data.Monoid (Monoid(..))
import Data.Serialize (Serialize(..))
import Data.Typeable (Typeable)
import Data.Word (Word)

{-| An unpacked 'Word' whose 'Monoid' instance is addition. -}
data WordSum = WordSum { getWordSum :: {-# UNPACK #-} !Word } deriving (Eq,Show,Typeable)

instance NFData WordSum where
  rnf (WordSum x) = x `seq` ()

{-| This instance sums the two contained 'Word's. -}
instance Semigroup WordSum where
    WordSum x <> WordSum y = WordSum (x+y)

{-| This instance has the value 0. -}
instance Monoid WordSum where
    mempty = WordSum 0

{-| This instance is equivalent to the instance for 'Word'. -}
instance Serialize WordSum where
    put = put . getWordSum
    get = fmap WordSum get
