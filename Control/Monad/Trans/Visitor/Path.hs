-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1201: * @file Control/Monad/Trans/Visitor/Path.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1202: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Path where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1203: ** << Import needed modules >>
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>))
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust)
import Data.Sequence (Seq,viewl,ViewL(..))
import qualified Data.SequentialIndex as SequentialIndex
import Data.SequentialIndex (SequentialIndex)
import Data.Serialize (Serialize(),decode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923120247.1204: ** Types
-- @+node:gcross.20111019113757.1409: *3* VisitorLabel
newtype VisitorLabel = VisitorLabel { unwrapVisitorLabel :: SequentialIndex } deriving (Eq,Ord)
-- @+node:gcross.20110923120247.1205: *3* VisitorPath
type VisitorPath = Seq VisitorStep
-- @+node:gcross.20111019113757.1250: *3* VisitorSolution
data VisitorSolution α = VisitorSolution VisitorLabel α
-- @+node:gcross.20110923120247.1206: *3* VisitorStep
data VisitorStep =
    CacheStep ByteString
 |  ChoiceStep WhichBranchActive
-- @+node:gcross.20110923120247.1209: *3* VisitorWalkError
data VisitorWalkError =
    VisitorTerminatedBeforeEndOfWalk
  | ChoiceStepAtCachePoint
  | CacheStepAtChoicePoint
  deriving (Eq,Show,Typeable)

instance Exception VisitorWalkError
-- @+node:gcross.20111019113757.1405: *3* WhichBranchActive
data WhichBranchActive =
    LeftBranchActive
  | RightBranchActive
  deriving (Eq,Read,Show)
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
-- @+node:gcross.20111019113757.1407: *3* labelTransformerForBranch
labelTransformerForBranch :: WhichBranchActive → (VisitorLabel → VisitorLabel)
labelTransformerForBranch LeftBranchActive = leftChildLabel
labelTransformerForBranch RightBranchActive = rightChildLabel
-- @+node:gcross.20111019113757.1414: *3* leftChildLabel
leftChildLabel :: VisitorLabel → VisitorLabel
leftChildLabel = VisitorLabel . fromJust . SequentialIndex.leftChild . unwrapVisitorLabel
-- @+node:gcross.20111019113757.1403: *3* oppositeBranchOf
oppositeBranchOf :: WhichBranchActive → WhichBranchActive
oppositeBranchOf LeftBranchActive = RightBranchActive
oppositeBranchOf RightBranchActive = LeftBranchActive
-- @+node:gcross.20111019113757.1416: *3* rightChildLabel
rightChildLabel :: VisitorLabel → VisitorLabel
rightChildLabel = VisitorLabel . fromJust . SequentialIndex.rightChild . unwrapVisitorLabel
-- @+node:gcross.20111019113757.1413: *3* rootLabel
rootLabel :: VisitorLabel
rootLabel = VisitorLabel SequentialIndex.root
-- @+node:gcross.20111019113757.1399: *3* walkVisitorDownLabel
walkVisitorDownLabel :: VisitorLabel → Visitor α → Visitor α
walkVisitorDownLabel label = runIdentity . walkVisitorTDownLabel label
-- @+node:gcross.20110923164140.1190: *3* walkVisitorDownPath
walkVisitorDownPath :: VisitorPath → Visitor α → Visitor α
walkVisitorDownPath path = runIdentity . walkVisitorTDownPath path
-- @+node:gcross.20111019113757.1395: *3* walkVisitorTDownLabel
walkVisitorTDownLabel :: Monad m ⇒ VisitorLabel → VisitorT m α → m (VisitorT m α)
walkVisitorTDownLabel label = go rootLabel
  where
    go parent visitor
      | parent == label = return visitor
      | otherwise =
          (viewT . unwrapVisitorT) visitor >>= \view → case view of
            Return x → throw VisitorTerminatedBeforeEndOfWalk
            Null :>>= _ → throw VisitorTerminatedBeforeEndOfWalk
            Cache mx :>>= k → mx >>= walkVisitorTDownLabel parent . VisitorT . k
            Choice left right :>>= k →
                if label < parent
                then
                    walkVisitorTDownLabel
                        (leftChildLabel parent)
                        (left >>= VisitorT . k)
                else
                    walkVisitorTDownLabel
                        (rightChildLabel parent)
                        (right >>= VisitorT . k)
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
        (Cache _ :>>= _,ChoiceStep _) →
            throw ChoiceStepAtCachePoint
        (Choice left _ :>>= k,ChoiceStep LeftBranchActive) →
            walkVisitorTDownPath tail (left >>= VisitorT . k)
        (Choice _ right :>>= k,ChoiceStep RightBranchActive) →
            walkVisitorTDownPath tail (right >>= VisitorT . k)
        (Choice _ _ :>>= _,CacheStep _) →
            throw CacheStepAtChoicePoint
-- @-others
-- @-leo