-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Examples.Tree where

-- Imports {{{
import Control.Applicative ((<$>),(<*>))
import Control.Monad (MonadPlus,msum)

import Data.List (genericReplicate)
import Data.Word (Word)

import System.Console.CmdTheLine

import Text.PrettyPrint (text)

import Control.Visitor (Visitor)
import Control.Visitor.Utils.WordSum
-- }}}

-- Types {{{
newtype Arity = Arity { getArity :: Word } deriving (Eq,Show)

data ArityAndDepth = ArityAndDepth -- {{{
    {   arity :: !Word
    ,   depth :: !Word
    } deriving (Eq, Show)
-- }}}
-- }}}

-- Instances {{{
instance ArgVal Arity where -- {{{
    converter = (parseArity,prettyArity)
      where
        (parseWord,prettyWord) = converter
        parseArity =
            either Left (\n →
                if n >= 2
                    then Right . Arity $ n
                    else Left . text $ "tree arity must be at least 2 (not " ++ show n ++ ")"
            )
            .
            parseWord
        prettyArity = prettyWord . getArity
instance ArgVal (Maybe Arity) where
    converter = just
-- }}}
-- }}}

-- Values {{{
makeArityAndDepthTermAtPositions :: Int → Int → Term ArityAndDepth
makeArityAndDepthTermAtPositions arity_position depth_position =
    formArityAndDepth
    <$> (required $
         pos arity_position
             Nothing
             posInfo
               { posName = "ARITY"
               , posDoc = "tree arity"
               }
        )
    <*> (required $
         pos depth_position
             Nothing
             posInfo
               { posName = "DEPTH"
               , posDoc = "tree depth (depth 0 means 1 level)"
               }
        )
-- }}}

-- Functions {{{

formArityAndDepth :: Arity → Word → ArityAndDepth -- {{{
formArityAndDepth (Arity arity) depth = ArityAndDepth{..}
-- }}}

numberOfLeaves :: Word → Word → Word -- {{{
numberOfLeaves arity depth = arity^depth
-- }}}

tree :: MonadPlus m ⇒ α → Word → Word → m α -- {{{
tree leaf arity depth
  | depth == 0 = return leaf
  | arity > 0  = msum . genericReplicate arity $ tree leaf arity (depth-1)
  | otherwise  = error "arity must be a positive integer"
{-# SPECIALIZE tree :: α → Word → Word → [α] #-}
{-# SPECIALIZE tree :: α → Word → Word → Visitor α #-}
-- }}}

trivialTree :: MonadPlus m ⇒ Word → Word → m WordSum -- {{{
trivialTree = tree (WordSum 1)
{-# SPECIALIZE trivialTree :: Word → Word → [WordSum] #-}
{-# SPECIALIZE trivialTree :: Word → Word → Visitor WordSum #-}
-- }}}

-- }}}