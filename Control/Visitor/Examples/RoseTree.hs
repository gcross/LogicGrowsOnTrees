-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Examples.RoseTree where

-- Imports {{{
import Control.Applicative ((<$>),(<*>))

import Data.List (genericReplicate)
import Data.Semiring (Semiring(..))
import Data.Tree (Tree(..))
import Data.Word (Word)

import System.Console.CmdTheLine

import Text.PrettyPrint (text)

import Control.Visitor.Utils.WordSum (WordSum(..))
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

checkArityAndDepth :: (Word → Word → α) → Word → Word → α -- {{{
checkArityAndDepth f arity depth
  | arity == 0 && depth /= 0
     = error $ "arity is zero, but depth (" ++ show depth ++ ") is non-zero"
  | otherwise
     = f arity depth
-- }}}

computeCorrectTrivialTreeSumOverPathsToLeaves :: Word → Word → Word -- {{{
computeCorrectTrivialTreeSumOverPathsToLeaves = checkArityAndDepth (^)
-- }}}

computeCorrectTrivialTreeSumOverNodes :: Word → Word → Word -- {{{
computeCorrectTrivialTreeSumOverNodes = checkArityAndDepth $ \arity depth → (arity^(depth+1) - 1) `div` (arity - 1)
-- }}}

formArityAndDepth :: Arity → Word → ArityAndDepth -- {{{
formArityAndDepth (Arity arity) depth = ArityAndDepth{..}
-- }}}

generateTrivialTree :: Word → Word → Tree WordSum -- {{{
generateTrivialTree = checkArityAndDepth $ \arity →
    let go1 n = Node munit $ go2 n
        go2 0 = []
        go2 n = genericReplicate arity $ go1 (n-1)
    in go1 
-- }}} 

-- }}}
