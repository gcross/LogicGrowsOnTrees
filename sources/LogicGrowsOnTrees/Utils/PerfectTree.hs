{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This modules contains utility functions for constructing perfect trees for
    use in some of the tests and examples.
 -}
module LogicGrowsOnTrees.Utils.PerfectTree
    (
    -- * Tree generators
      perfectTree
    , trivialPerfectTree
    , numberOfLeaves
    -- * Arity and depth parameters
    , Arity(..)
    , ArityAndDepth(..)
    , arity_and_depth_parser
    , arity_reader
    , formArityAndDepth
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (MonadPlus,msum)

import Data.List (genericReplicate)
import Data.Serialize (Serialize)
import Data.Word (Word)

import GHC.Generics (Generic)

import Options.Applicative (Parser, ReadM, argument, auto, help, metavar)

import LogicGrowsOnTrees (Tree)
import LogicGrowsOnTrees.Utils.WordSum

--------------------------------------------------------------------------------
-------------------------- Arity and depth parameters --------------------------
--------------------------------------------------------------------------------

{-| Newtype wrapper for arities that has an 'ArgVal' instance that enforces that
    the arity be at least 2.
 -}
newtype Arity = Arity { getArity :: Word } deriving (Eq,Show,Serialize)

arity_reader :: ReadM Arity
arity_reader = auto >>= \n →
    if n >= 2
        then pure $ Arity n
        else fail $ "tree arity must be at least 2 (not " ++ show n ++ ")"

{-| Datatype representing the arity and depth of a tree, used for command line
    argument processing (see 'makeArityAndDepthTermAtPositions').
 -}
data ArityAndDepth = ArityAndDepth
    {   arity :: !Word
    ,   depth :: !Word
    } deriving (Eq, Generic, Show)
instance Serialize ArityAndDepth where

arity_and_depth_parser :: Parser ArityAndDepth
arity_and_depth_parser =
    ArityAndDepth
        <$> (argument auto $ mconcat
                [ metavar "ARITY"
                , help "tree arity"
                ]
            )
        <*> (argument auto $ mconcat
                [ metavar "DEPTH"
                , help "tree depth"
                ]
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
    Word {-^ the arity of the tree (i.e., number of branches at each internal node) -} →
    Word {-^ the depth of the tree -} →
    m α
perfectTree leaf arity depth
  | depth == 0 = return leaf
  | arity > 0  = msum . genericReplicate arity $ perfectTree leaf arity (depth-1)
  | otherwise  = error "arity must be a positive integer"
{-# SPECIALIZE perfectTree :: α → Word → Word → [α] #-}
{-# SPECIALIZE perfectTree :: α → Word → Word → Tree α #-}

{-| Like 'perfectTree' but with @WordSum 1@ at the leaves. -}
trivialPerfectTree ::
    MonadPlus m ⇒
    Word {-^ the arity of the tree (i.e., number of branches at each internal node) -} →
    Word {-^ the depth of the tree -} →
    m WordSum
trivialPerfectTree = perfectTree (WordSum 1)
{-# SPECIALIZE trivialPerfectTree :: Word → Word → [WordSum] #-}
{-# SPECIALIZE trivialPerfectTree :: Word → Word → Tree WordSum #-}

{-| Computes the number of leaves in a perfect tree.  It returns a value of type
    'Word' so that it can be easily compared to the 'WordSum' value returned by
    the tree generators, but a consequence of this is that it will overflow if
    the arity and/or depth arguments are too large.
 -}
numberOfLeaves ::
    Word {-^ the arity of the tree (i.e., number of branches at each internal node) -} →
    Word {-^ the depth of the tree -} →
    Word
numberOfLeaves arity depth = arity^depth
