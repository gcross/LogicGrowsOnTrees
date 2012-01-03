-- @+leo-ver=5-thin
-- @+node:gcross.20111029192420.1332: * @file Label.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111029192420.1333: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Label where

-- @+<< Import needed modules >>
-- @+node:gcross.20111029192420.1334: ** << Import needed modules >>
import Control.Exception (throw)
import Control.Monad ((>=>),liftM2)
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.Composition
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Foldable as Fold
import Data.Foldable (Foldable)
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import Data.SequentialIndex (SequentialIndex,root,leftChild,rightChild)

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111029192420.1335: ** Types
-- @+node:gcross.20111019113757.1409: *3* VisitorLabel
newtype VisitorLabel = VisitorLabel { unwrapVisitorLabel :: SequentialIndex } deriving (Eq)
-- @+node:gcross.20111019113757.1250: *3* VisitorSolution
data VisitorSolution α = VisitorSolution
    {   visitorSolutionLabel :: VisitorLabel
    ,   visitorSolutionResult :: α
    } deriving (Eq,Ord,Show)
-- @+node:gcross.20111028153100.1295: ** Instances
-- @+node:gcross.20111029192420.1361: *3* Monoid VisitorLabel
instance Monoid VisitorLabel where
    mempty = rootLabel
    xl@(VisitorLabel x) `mappend` yl@(VisitorLabel y)
      | x == root = yl
      | y == root = xl
      | otherwise = VisitorLabel $ go y root x
      where
        go original_label current_label product_label
          | current_label == original_label = product_label
        -- Note:  the following is counter-intuitive, but it makes sense if you think of it as
        --        being where you need to go to get to the original label instead of where you
        --        currently are with respect to the original label
          | current_label > original_label = (go original_label `on` (fromJust . leftChild)) current_label product_label
          | current_label < original_label = (go original_label `on` (fromJust . rightChild)) current_label product_label
-- @+node:gcross.20111116214909.1376: *3* Ord VisitorLabel
instance Ord VisitorLabel where
    compare = compare `on` branchingFromLabel
-- @+node:gcross.20111028153100.1296: *3* Show VisitorLabel
instance Show VisitorLabel where
    show = fmap (\branch → case branch of {LeftBranch → 'L'; RightBranch → 'R'}) . branchingFromLabel
-- @+node:gcross.20111029192420.1336: ** Functions
-- @+node:gcross.20111020151748.1288: *3* applyCheckpointCursorToLabel
applyCheckpointCursorToLabel :: VisitorCheckpointCursor → VisitorLabel → VisitorLabel
applyCheckpointCursorToLabel (viewl → EmptyL) = id
applyCheckpointCursorToLabel (viewl → step :< rest) =
    applyCheckpointCursorToLabel rest
    .
    case step of
        CacheCheckpointD _ → id
        ChoiceCheckpointD active_branch _ → labelTransformerForBranch active_branch
-- @+node:gcross.20111019113757.1400: *3* applyContextToLabel
applyContextToLabel :: VisitorTContext m α → VisitorLabel → VisitorLabel
applyContextToLabel (viewl → EmptyL) = id
applyContextToLabel (viewl → step :< rest) =
    applyContextToLabel rest
    .
    case step of
        BranchContextStep branch_active → labelTransformerForBranch branch_active
        CacheContextStep _ → id
        LeftChoiceContextStep _ _ → leftChildLabel
-- @+node:gcross.20111020151748.1291: *3* applyPathToLabel
applyPathToLabel :: VisitorPath → VisitorLabel → VisitorLabel
applyPathToLabel (viewl → EmptyL) = id
applyPathToLabel (viewl → step :< rest) =
    applyPathToLabel rest
    .
    case step of
        ChoiceStep active_branch → labelTransformerForBranch active_branch
        CacheStep _ → id
-- @+node:gcross.20111029212714.1353: *3* branchingFromLabel
branchingFromLabel :: VisitorLabel → [Branch]
branchingFromLabel = go root . unwrapVisitorLabel
  where
    go current_label original_label
      | current_label == original_label = []
      | current_label > original_label = LeftBranch:go (fromJust . leftChild $ current_label) original_label
      | current_label < original_label = RightBranch:go (fromJust . rightChild $ current_label) original_label
-- @+node:gcross.20111028153100.1294: *3* labelFromBranching
labelFromBranching :: Foldable t ⇒ t Branch → VisitorLabel
labelFromBranching = Fold.foldl' (flip labelTransformerForBranch) rootLabel
-- @+node:gcross.20111019113757.1412: *3* labelFromContext
labelFromContext :: VisitorTContext m α → VisitorLabel
labelFromContext = flip applyContextToLabel rootLabel
-- @+node:gcross.20111020151748.1292: *3* labelFromPath
labelFromPath :: VisitorPath → VisitorLabel
labelFromPath = flip applyPathToLabel rootLabel
-- @+node:gcross.20111019113757.1407: *3* labelTransformerForBranch
labelTransformerForBranch :: Branch → (VisitorLabel → VisitorLabel)
labelTransformerForBranch LeftBranch = leftChildLabel
labelTransformerForBranch RightBranch = rightChildLabel
-- @+node:gcross.20111019113757.1414: *3* leftChildLabel
leftChildLabel :: VisitorLabel → VisitorLabel
leftChildLabel = VisitorLabel . fromJust . leftChild . unwrapVisitorLabel
-- @+node:gcross.20111019113757.1416: *3* rightChildLabel
rightChildLabel :: VisitorLabel → VisitorLabel
rightChildLabel = VisitorLabel . fromJust . rightChild . unwrapVisitorLabel
-- @+node:gcross.20111019113757.1413: *3* rootLabel
rootLabel :: VisitorLabel
rootLabel = VisitorLabel root
-- @+node:gcross.20111029192420.1338: *3* runVisitorTWithLabelsAndGatherResults
runVisitorTWithLabelsAndGatherResults :: Monad m ⇒ VisitorT m α → m [VisitorSolution α]
runVisitorTWithLabelsAndGatherResults = runVisitorTWithStartingLabel rootLabel
-- @+node:gcross.20111029192420.1358: *3* runVisitorTWithStartingLabel
runVisitorTWithStartingLabel :: Monad m ⇒ VisitorLabel → VisitorT m α → m [VisitorSolution α]
runVisitorTWithStartingLabel label =
    viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → return [VisitorSolution label x]
        (Cache mx :>>= k) → mx >>= runVisitorTWithStartingLabel label . VisitorT . k
        (Choice left right :>>= k) →
            liftM2 (++)
                (runVisitorTWithStartingLabel (leftChildLabel label) $ left >>= VisitorT . k)
                (runVisitorTWithStartingLabel (rightChildLabel label) $ right >>= VisitorT . k)
        (Null :>>= _) → return []
-- @+node:gcross.20111029192420.1340: *3* runVisitorWithLabels
runVisitorWithLabels :: Visitor α → [VisitorSolution α]
runVisitorWithLabels = runIdentity . runVisitorTWithLabelsAndGatherResults
-- @+node:gcross.20111029192420.1360: *3* runVisitorWithStartingLabel
runVisitorWithStartingLabel :: VisitorLabel → Visitor α → [VisitorSolution α]
runVisitorWithStartingLabel = runIdentity .* runVisitorTWithStartingLabel
-- @+node:gcross.20111117140347.1430: *3* solutionsToMap
solutionsToMap :: Foldable t ⇒ t (VisitorSolution α) → Map VisitorLabel α
solutionsToMap = Fold.foldl' (flip $ \(VisitorSolution label solution) → Map.insert label solution) Map.empty
-- @+node:gcross.20111019113757.1399: *3* walkVisitorDownLabel
walkVisitorDownLabel :: VisitorLabel → Visitor α → Visitor α
walkVisitorDownLabel label = runIdentity . walkVisitorTDownLabel label
-- @+node:gcross.20111019113757.1395: *3* walkVisitorTDownLabel
walkVisitorTDownLabel :: Monad m ⇒ VisitorLabel → VisitorT m α → m (VisitorT m α)
walkVisitorTDownLabel (VisitorLabel label) = go root
  where
    go parent visitor
      | parent == label = return visitor
      | otherwise =
          (viewT . unwrapVisitorT) visitor >>= \view → case view of
            Return x → throw VisitorTerminatedBeforeEndOfWalk
            Null :>>= _ → throw VisitorTerminatedBeforeEndOfWalk
            Cache mx :>>= k → mx >>= go parent . VisitorT . k
            Choice left right :>>= k →
                if parent > label
                then
                    go
                        (fromJust . leftChild $ parent)
                        (left >>= VisitorT . k)
                else
                    go
                        (fromJust . rightChild $ parent)
                        (right >>= VisitorT . k)
-- @-others
-- @-leo
