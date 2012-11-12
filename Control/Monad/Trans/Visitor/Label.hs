
-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Label where

-- Imports {{{
import Control.Exception (throw)
import Control.Monad ((>=>),liftM2)
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.Composition
import qualified Data.Map as Map
import Data.Map (Map)
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
-- }}}

-- Types {{{

newtype VisitorLabel = VisitorLabel { unwrapVisitorLabel :: SequentialIndex } deriving (Eq)

data VisitorSolution α = VisitorSolution
    {   visitorSolutionLabel :: VisitorLabel
    ,   visitorSolutionResult :: α
    } deriving (Eq,Ord,Show)

-- }}}

-- Instances {{{

instance Monoid VisitorLabel where -- {{{
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
-- }}}

instance Ord VisitorLabel where -- {{{
    compare = compare `on` branchingFromLabel
-- }}}

instance Show VisitorLabel where -- {{{
    show = fmap (\branch → case branch of {LeftBranch → 'L'; RightBranch → 'R'}) . branchingFromLabel
-- }}}

-- }}}

-- Functions {{{

applyCheckpointCursorToLabel :: VisitorCheckpointCursor → VisitorLabel → VisitorLabel -- {{{
applyCheckpointCursorToLabel (viewl → EmptyL) = id
applyCheckpointCursorToLabel (viewl → step :< rest) =
    applyCheckpointCursorToLabel rest
    .
    case step of
        CacheCheckpointD _ → id
        ChoiceCheckpointD active_branch _ → labelTransformerForBranch active_branch
-- }}}

applyContextToLabel :: VisitorTContext m α → VisitorLabel → VisitorLabel -- {{{
applyContextToLabel (viewl → EmptyL) = id
applyContextToLabel (viewl → step :< rest) =
    applyContextToLabel rest
    .
    case step of
        BranchContextStep branch_active → labelTransformerForBranch branch_active
        CacheContextStep _ → id
        LeftChoiceContextStep _ _ → leftChildLabel
-- }}}

applyPathToLabel :: VisitorPath → VisitorLabel → VisitorLabel -- {{{
applyPathToLabel (viewl → EmptyL) = id
applyPathToLabel (viewl → step :< rest) =
    applyPathToLabel rest
    .
    case step of
        ChoiceStep active_branch → labelTransformerForBranch active_branch
        CacheStep _ → id
-- }}}

branchingFromLabel :: VisitorLabel → [Branch] -- {{{
branchingFromLabel = go root . unwrapVisitorLabel
  where
    go current_label original_label
      | current_label == original_label = []
      | current_label > original_label = LeftBranch:go (fromJust . leftChild $ current_label) original_label
      | current_label < original_label = RightBranch:go (fromJust . rightChild $ current_label) original_label
-- }}}

labelFromBranching :: Foldable t ⇒ t Branch → VisitorLabel -- {{{
labelFromBranching = Fold.foldl' (flip labelTransformerForBranch) rootLabel
-- }}}

labelFromContext :: VisitorTContext m α → VisitorLabel -- {{{
labelFromContext = flip applyContextToLabel rootLabel
-- }}}

labelFromPath :: VisitorPath → VisitorLabel -- {{{
labelFromPath = flip applyPathToLabel rootLabel
-- }}}

labelTransformerForBranch :: Branch → (VisitorLabel → VisitorLabel) -- {{{
labelTransformerForBranch LeftBranch = leftChildLabel
labelTransformerForBranch RightBranch = rightChildLabel
-- }}}

leftChildLabel :: VisitorLabel → VisitorLabel -- {{{
leftChildLabel = VisitorLabel . fromJust . leftChild . unwrapVisitorLabel
-- }}}

rightChildLabel :: VisitorLabel → VisitorLabel -- {{{
rightChildLabel = VisitorLabel . fromJust . rightChild . unwrapVisitorLabel
-- }}}

rootLabel :: VisitorLabel -- {{{
rootLabel = VisitorLabel root
-- }}}

runVisitorTWithLabelsAndGatherResults :: Monad m ⇒ VisitorT m α → m [VisitorSolution α] -- {{{
runVisitorTWithLabelsAndGatherResults = runVisitorTWithStartingLabel rootLabel
-- }}}

runVisitorTWithStartingLabel :: Monad m ⇒ VisitorLabel → VisitorT m α → m [VisitorSolution α] -- {{{
runVisitorTWithStartingLabel label =
    viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → return [VisitorSolution label x]
        (Cache mx :>>= k) → mx >>= maybe (return []) (runVisitorTWithStartingLabel label . VisitorT . k)
        (Choice left right :>>= k) →
            liftM2 (++)
                (runVisitorTWithStartingLabel (leftChildLabel label) $ left >>= VisitorT . k)
                (runVisitorTWithStartingLabel (rightChildLabel label) $ right >>= VisitorT . k)
        (Null :>>= _) → return []
-- }}}

runVisitorWithLabels :: Visitor α → [VisitorSolution α] -- {{{
runVisitorWithLabels = runIdentity . runVisitorTWithLabelsAndGatherResults
-- }}}

runVisitorWithStartingLabel :: VisitorLabel → Visitor α → [VisitorSolution α] -- {{{
runVisitorWithStartingLabel = runIdentity .* runVisitorTWithStartingLabel
-- }}}

solutionsToMap :: Foldable t ⇒ t (VisitorSolution α) → Map VisitorLabel α -- {{{
solutionsToMap = Fold.foldl' (flip $ \(VisitorSolution label solution) → Map.insert label solution) Map.empty
-- }}}

walkVisitorDownLabel :: VisitorLabel → Visitor α → Visitor α -- {{{
walkVisitorDownLabel label = runIdentity . walkVisitorTDownLabel label
-- }}}

walkVisitorTDownLabel :: Monad m ⇒ VisitorLabel → VisitorT m α → m (VisitorT m α) -- {{{
walkVisitorTDownLabel (VisitorLabel label) = go root
  where
    go parent visitor
      | parent == label = return visitor
      | otherwise =
          (viewT . unwrapVisitorT) visitor >>= \view → case view of
            Return x → throw VisitorTerminatedBeforeEndOfWalk
            Null :>>= _ → throw VisitorTerminatedBeforeEndOfWalk
            Cache mx :>>= k → mx >>= maybe (throw VisitorTerminatedBeforeEndOfWalk) (go parent . VisitorT . k)
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
-- }}}

-- }}}
