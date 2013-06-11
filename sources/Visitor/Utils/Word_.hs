{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module provides a temporary 'ArgVal' instance for 'Word' (via a newtype
    wrapper 'Word_' until) 'cmdtheline' releases a new version that includes an
    instance for 'Word' itself, after which this module will be dropped.
 -}
module Visitor.Utils.Word_ where

import Data.Word
import System.Console.CmdTheLine
import Text.PrettyPrint

{-| Newtype wrapper used to indirectly provide an 'ArgVal' instance for Word. -}
newtype Word_ = Word_ { getWord :: Word } deriving (Eq,Show)

instance ArgVal Word_ where
    converter = (parseWord,prettyWord)
      where
        (parseInt,prettyInt) = converter
        parseWord =
            either Left (\n →
                if n >= (0::Int)
                    then Right . Word_ . fromIntegral $ n
                    else Left . text $ "non-negative argument required (not " ++ show n ++ ")"
            )
            .
            parseInt
        prettyWord = prettyInt . fromIntegral . getWord

instance ArgVal (Maybe Word_) where
    converter = just
