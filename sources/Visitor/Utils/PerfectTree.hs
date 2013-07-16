{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This modules contains utility functions for constructing perfect trees for
    use in some of the tests and examples.
 -}
module Visitor.Utils.PerfectTree
    (
    -- * Tree generators
      perfectTree
    , trivialPerfectTree
    , numberOfLeaves
    -- * Arity and depth parameters
    , Arity(..)
    , ArityAndDepth(..)
    , makeArityAndDepthTermAtPositions
    , formArityAndDepth
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (MonadPlus,msum)

import Data.List (genericReplicate)
import Data.Word (Word)

import System.Console.CmdTheLine

import Text.PrettyPrint (text)

import Visitor (Tree)
import Visitor.Utils.Word_
import Visitor.Utils.WordSum

--------------------------------------------------------------------------------
-------------------------- Arity and depth parameters --------------------------
--------------------------------------------------------------------------------

{-| Newtype wrapper for arities that has an 'ArgVal' instance that enforces that
    the arity be at least 2.
 -}
newtype Arity = Arity { getArity :: Word } deriving (Eq,Show)

instance ArgVal Arity where
    converter = (parseArity,prettyArity)
      where
        (parseWord_,prettyWord_) = converter
        parseArity =
            either Left (\(Word_ n) →
                if n >= 2
                    then Right . Arity $ n
                    else Left . text $ "tree arity must be at least 2 (not " ++ show n ++ ")"
            )
            .
            parseWord_
        prettyArity = prettyWord_ . Word_ . getArity
instance ArgVal (Maybe Arity) where
    converter = just

{-| Datatype representing the arity and depth of a tree, used for command line
    argument processing (see 'makeArityAndDepthTermAtPositions').
 -}
data ArityAndDepth = ArityAndDepth
    {   arity :: !Word
    ,   depth :: !Word
    } deriving (Eq, Show)

{-| Constructs a configuration term that expects the arity and depth to be at
    the given command line argument positions.
 -}
makeArityAndDepthTermAtPositions ::
    Int {-^ the position of the arity parameter in the command line -} →
    Int {-^ the position of the depth parameter in the command line -} →
    Term ArityAndDepth
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
    <*> (fmap getWord . required $
         pos depth_position
             Nothing
             posInfo
               { posName = "DEPTH"
               , posDoc = "tree depth (depth 0 means 1 level)"
               }
        )

{-| A convenience function used when you have an value of type 'Arity' for the
    arity of the tree rather than a value of type 'Word' and want to construct
    a value of type 'ArityAndDepth'.
 -}
formArityAndDepth :: Arity → Word → ArityAndDepth
formArityAndDepth (Arity arity) depth = ArityAndDepth{..}

--------------------------------------------------------------------------------
------------------------------- Tree generators --------------------------------
--------------------------------------------------------------------------------

{-| Generate a perfectly balanced tree with the given leaf value, arity, and leaf. -}
perfectTree ::
    MonadPlus m ⇒
    α {-^ the value to place at the leaves -} →
    Word {-^ the arity of the tree (i.e., number of branches) -} →
    Word {-^ the depth of the tree -} →
    m α {-^ the tree -}
perfectTree leaf arity depth
  | depth == 0 = return leaf
  | arity > 0  = msum . genericReplicate arity $ perfectTree leaf arity (depth-1)
  | otherwise  = error "arity must be a positive integer"
{-# SPECIALIZE perfectTree :: α → Word → Word → [α] #-}
{-# SPECIALIZE perfectTree :: α → Word → Word → Tree α #-}

{-| 'tree' with @WordSum 1@ at the leaves. -}
trivialPerfectTree ::
    MonadPlus m ⇒
    Word {-^ the arity of the tree (i.e., number of branches) -} →
    Word {-^ the depth of the tree -} →
    m WordSum {-^ the tree -}
trivialPerfectTree = perfectTree (WordSum 1)
{-# SPECIALIZE trivialPerfectTree :: Word → Word → [WordSum] #-}
{-# SPECIALIZE trivialPerfectTree :: Word → Word → Tree WordSum #-}

{-| Computes the number of leaves in a tree.  It returns a value of type 'Word'
    so that it can be easily compared to the 'WordSum' value returned by the
    tre generators, but a consequence of this is that it will blow up if the
    arity and/or depth arguments are too large.
 -}
numberOfLeaves ::
    Word {-^ the arity (i.e., number of branches) of the tree -} →
    Word {-^ the depth of the tree -} →
    Word {-^ the number of leaves in the tree -}
numberOfLeaves arity depth = arity^depth
