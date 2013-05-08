-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Utils.Word_ where

-- Imports {{{
import Data.Word
import System.Console.CmdTheLine
import Text.PrettyPrint
-- }}}

-- Types {{{
newtype Word_ = Word_ { getWord :: Word } deriving (Eq,Show)
-- }}}

-- Instances {{{
instance ArgVal Word_ where -- {{{
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
-- }}}
instance ArgVal (Maybe Word_) where -- {{{
    converter = just
-- }}}
-- }}}
