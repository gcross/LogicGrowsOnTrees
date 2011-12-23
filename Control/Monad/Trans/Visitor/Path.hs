-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1201: * @file Path.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1202: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Path where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1203: ** << Import needed modules >>
import Control.Exception (throw)
import Control.Monad ((>=>))
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq,viewl,ViewL(..))
import Data.Serialize (Serialize(),decode)

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Label
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923120247.1204: ** Types
-- @+node:gcross.20110923120247.1205: *3* VisitorPath
type VisitorPath = Seq VisitorStep
-- @+node:gcross.20110923120247.1206: *3* VisitorStep
data VisitorStep =
    CacheStep ByteString
 |  ChoiceStep Branch
 deriving (Eq,Show)
-- @+node:gcross.20110923120247.1208: ** Functions
-- @+node:gcross.20111020151748.1291: *3* applyPathToLabel
applyPathToLabel :: VisitorPath → VisitorLabel → VisitorLabel
applyPathToLabel (viewl → EmptyL) = id
applyPathToLabel (viewl → step :< rest) =
    applyPathToLabel rest
    .
    case step of
        ChoiceStep active_branch → labelTransformerForBranch active_branch
        CacheStep _ → id
-- @+node:gcross.20111020151748.1292: *3* labelFromPath
labelFromPath :: VisitorPath → VisitorLabel
labelFromPath = flip applyPathToLabel rootLabel
-- @+node:gcross.20110923164140.1190: *3* walkVisitorDownPath
walkVisitorDownPath :: VisitorPath → Visitor α → Visitor α
walkVisitorDownPath path = runIdentity . walkVisitorTDownPath path
-- @+node:gcross.20110923120247.1207: *3* walkVisitorTDownPath
walkVisitorTDownPath :: Monad m ⇒ VisitorPath → VisitorT m α → m (VisitorT m α)
walkVisitorTDownPath (viewl → EmptyL) = return
walkVisitorTDownPath path@(viewl → step :< tail) =
    viewT . unwrapVisitorT >=> \view → case (view,step) of
        (Return x,_) →
            throw VisitorTerminatedBeforeEndOfWalk
        (Null :>>= _,_) →
            throw VisitorTerminatedBeforeEndOfWalk
        (Cache _ :>>= k,CacheStep cache) →
            walkVisitorTDownPath tail $ either error (VisitorT . k) (decode cache)
        (Choice left _ :>>= k,ChoiceStep LeftBranch) →
            walkVisitorTDownPath tail (left >>= VisitorT . k)
        (Choice _ right :>>= k,ChoiceStep RightBranch) →
            walkVisitorTDownPath tail (right >>= VisitorT . k)
        _ → throw PastVisitorIsInconsistentWithPresentVisitor
-- @-others
-- @-leo
