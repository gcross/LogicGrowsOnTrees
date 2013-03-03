-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Examples.RoseTree where

-- Imports {{{
import Control.Visitor.Utils.WordSum (WordSum(..))
import Data.List (genericReplicate)
import Data.Semiring (Semiring(..))
import Data.Tree (Tree(..))
import Data.Word (Word)
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

generateTrivialTree :: Word → Word → Tree WordSum -- {{{
generateTrivialTree = checkArityAndDepth $ \arity →
    let go1 n = Node munit $ go2 n
        go2 0 = []
        go2 n = genericReplicate arity $ go1 (n-1)
    in go1 
-- }}} 

-- }}}
