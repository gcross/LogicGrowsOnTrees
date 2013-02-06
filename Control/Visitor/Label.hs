-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Label where

-- Imports {{{
import Control.Applicative (Alternative(..),Applicative(..))
import Control.Exception (throw)
import Control.Monad (MonadPlus(..),(>=>),liftM2)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..),ask)

import Data.Composition
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Foldable as Fold
import Data.Foldable (Foldable)
import Data.Function (on)
import Data.Functor.Identity (Identity,runIdentity)
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import Data.SequentialIndex (SequentialIndex,root,leftChild,rightChild)

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Path
-- }}}

-- Classes {{{
class Monad m ⇒ MonadLabeled m where
    getLabel :: m VisitorLabel
-- }}}

-- Types {{{

newtype LabeledT m α = LabeledT { unwrapLabeledT :: ReaderT VisitorLabel m α }
    deriving (Applicative,Functor,Monad,MonadIO,MonadTrans)
newtype LabeledVisitorT m α = LabeledVisitorT { unwrapLabeledVisitorT :: LabeledT (VisitorT m) α }
    deriving (Alternative,Applicative,Functor,Monad,MonadIO,MonadLabeled,MonadPlus,MonadVisitor,Monoid)
type LabeledVisitorIO = LabeledVisitorT IO
type LabeledVisitor = LabeledVisitorT Identity

newtype VisitorLabel = VisitorLabel { unwrapVisitorLabel :: SequentialIndex } deriving (Eq)

data VisitorSolution α = VisitorSolution
    {   visitorSolutionLabel :: VisitorLabel
    ,   visitorSolutionResult :: α
    } deriving (Eq,Ord,Show)

-- }}}

-- Instances {{{

instance (Alternative m, Monad m) ⇒ Alternative (LabeledT m) where -- {{{
    empty = LabeledT $ lift empty
    LabeledT left <|> LabeledT right = LabeledT . ReaderT $
        \branch → (runReaderT left (leftChildLabel branch)) <|> (runReaderT right (rightChildLabel branch))
-- }}}

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

instance Monad m ⇒ MonadLabeled (LabeledT m) where -- {{{
    getLabel = LabeledT $ ask
-- }}}

instance MonadTrans LabeledVisitorT where -- {{{
    lift = LabeledVisitorT . lift . lift
-- }}}

instance MonadPlus m ⇒ MonadPlus (LabeledT m) where -- {{{
    mzero = LabeledT $ lift mzero
    LabeledT left `mplus` LabeledT right = LabeledT . ReaderT $
        \branch → (runReaderT left (leftChildLabel branch)) `mplus` (runReaderT right (rightChildLabel branch))
-- }}}

instance MonadVisitor m ⇒ MonadVisitor (LabeledT m) where -- {{{
    cache = LabeledT . lift . cache
    cacheGuard = LabeledT . lift . cacheGuard
    cacheMaybe = LabeledT . lift . cacheMaybe
-- }}}

instance MonadVisitorTrans m ⇒ MonadVisitorTrans (LabeledT m) where -- {{{
    type NestedMonadInVisitor (LabeledT m) = NestedMonadInVisitor m
    runAndCache = LabeledT . lift . runAndCache
    runAndCacheGuard = LabeledT . lift . runAndCacheGuard
    runAndCacheMaybe = LabeledT . lift . runAndCacheMaybe
-- }}}

instance (Functor m, Monad m) ⇒ MonadVisitorTrans (LabeledVisitorT m) where -- {{{
    type NestedMonadInVisitor (LabeledVisitorT m) = m
    runAndCache = LabeledVisitorT . runAndCache
    runAndCacheGuard = LabeledVisitorT . runAndCacheGuard
    runAndCacheMaybe = LabeledVisitorT . runAndCacheMaybe
-- }}}

instance MonadPlus m ⇒ Monoid (LabeledT m α) where -- {{{
    mempty = mzero
    mappend = mplus
-- }}}

-- }}}

-- Functions {{{

applyCheckpointCursorToLabel :: CheckpointCursor → VisitorLabel → VisitorLabel -- {{{
applyCheckpointCursorToLabel (viewl → EmptyL) = id
applyCheckpointCursorToLabel (viewl → step :< rest) =
    applyCheckpointCursorToLabel rest
    .
    case step of
        CacheCheckpointD _ → id
        ChoiceCheckpointD active_branch _ → labelTransformerForBranch active_branch
-- }}}

applyContextToLabel :: Context m α → VisitorLabel → VisitorLabel -- {{{
applyContextToLabel (viewl → EmptyL) = id
applyContextToLabel (viewl → step :< rest) =
    applyContextToLabel rest
    .
    case step of
        CacheContextStep _ → id
        LeftBranchContextStep _ _ → leftChildLabel
        RightBranchContextStep → rightChildLabel
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

labelFromContext :: Context m α → VisitorLabel -- {{{
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

normalizeLabeledVisitor :: LabeledVisitor α → Visitor α -- {{{
normalizeLabeledVisitor = runLabeledT . unwrapLabeledVisitorT
-- }}}

normalizeLabeledVisitorT :: LabeledVisitorT m α → VisitorT m α -- {{{
normalizeLabeledVisitorT = runLabeledT . unwrapLabeledVisitorT
-- }}}

rightChildLabel :: VisitorLabel → VisitorLabel -- {{{
rightChildLabel = VisitorLabel . fromJust . rightChild . unwrapVisitorLabel
-- }}}

rootLabel :: VisitorLabel -- {{{
rootLabel = VisitorLabel root
-- }}}

runLabeledT :: LabeledT m α → m α -- {{{
runLabeledT = flip runReaderT rootLabel . unwrapLabeledT
-- }}}

runLabeledVisitor :: Monoid α ⇒ LabeledVisitor α → α -- {{{
runLabeledVisitor = runVisitor . runLabeledT . unwrapLabeledVisitorT
-- }}}

runLabeledVisitorT :: (Monoid α,Monad m) ⇒ LabeledVisitorT m α → m α -- {{{
runLabeledVisitorT = runVisitorT . runLabeledT . unwrapLabeledVisitorT
-- }}}

runLabeledVisitorTAndIgnoreResults :: Monad m ⇒ LabeledVisitorT m α → m () -- {{{
runLabeledVisitorTAndIgnoreResults = runVisitorTAndIgnoreResults . runLabeledT . unwrapLabeledVisitorT
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

sendVisitorDownLabel :: VisitorLabel → Visitor α → Visitor α -- {{{
sendVisitorDownLabel label = runIdentity . sendVisitorTDownLabel label
-- }}}

sendVisitorTDownLabel :: Monad m ⇒ VisitorLabel → VisitorT m α → m (VisitorT m α) -- {{{
sendVisitorTDownLabel (VisitorLabel label) = go root
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

solutionsToMap :: Foldable t ⇒ t (VisitorSolution α) → Map VisitorLabel α -- {{{
solutionsToMap = Fold.foldl' (flip $ \(VisitorSolution label solution) → Map.insert label solution) Map.empty
-- }}}

-- }}}
