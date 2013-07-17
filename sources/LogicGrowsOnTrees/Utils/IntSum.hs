{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains a type that specializes the 'Sum' 'Monoid' to 'Int'. -}
module LogicGrowsOnTrees.Utils.IntSum where

import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Serialize (Serialize(..))
import Data.Typeable (Typeable)

{-| A datatype that contains an unboxed 'Int'. -}
data IntSum = IntSum { getIntSum :: {-# UNPACK #-} !Int } deriving (Eq,Show,Typeable)

{-| This instance sums the two contained 'Int's. -}
instance Monoid IntSum where
    mempty = IntSum 0
    IntSum x `mappend` IntSum y = IntSum (x+y)
    mconcat = foldl' mappend mempty

{-| This instances is equivalent to the instance for 'Int'. -}
instance Serialize IntSum where
    put = put . getIntSum
    get = fmap IntSum get
