-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Label where

-- Imports {{{
import Control.Applicative (Alternative(..),Applicative(..))
import Control.Exception (throw)
import Control.Monad (MonadPlus(..),(>=>),liftM,liftM2)
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
import Data.Function (on)
import Data.Functor.Identity (Identity,runIdentity)
import Data.Sequence (viewl,ViewL(..))
import Data.SequentialIndex (SequentialIndex,root,leftChild,rightChild)

import Visitor
import Visitor.Checkpoint
import Visitor.Path
-- }}}

-- Classes {{{
class Monad m ⇒ MonadLabeled m where
    getLabel :: m Label
-- }}}

-- Types {{{

newtype LabeledT m α = LabeledT { unwrapLabeledT :: ReaderT Label m α }
    deriving (Applicative,Functor,Monad,MonadIO,MonadTrans)
newtype LabeledTreeBuilderT m α = LabeledTreeBuilderT { unwrapLabeledTreeBuilderT :: LabeledT (TreeBuilderT m) α }
    deriving (Alternative,Applicative,Functor,Monad,MonadIO,MonadLabeled,MonadPlus,Monoid)
type LabeledTreeBuilderIO = LabeledTreeBuilderT IO
type LabeledTreeBuilder = LabeledTreeBuilderT Identity

newtype Label = Label { unwrapLabel :: SequentialIndex } deriving (Eq)

data Solution α = Solution
    {   solutionLabel :: Label
    ,   solutionResult :: α
    } deriving (Eq,Ord,Show)

-- }}}

-- Instances {{{

instance (Alternative m, Monad m) ⇒ Alternative (LabeledT m) where -- {{{
    empty = LabeledT $ lift empty
    LabeledT left <|> LabeledT right = LabeledT . ReaderT $
        \branch → (runReaderT left (leftChildLabel branch)) <|> (runReaderT right (rightChildLabel branch))
-- }}}

instance Monoid Label where -- {{{
    mempty = rootLabel
    xl@(Label x) `mappend` yl@(Label y)
      | x == root = yl
      | y == root = xl
      | otherwise = Label $ go y root x
      where
        go original_label current_label product_label =
            case current_label `compare` original_label of
                EQ → product_label
            -- Note:  the following is counter-intuitive, but it makes sense if you think of it as
            --        being where you need to go to get to the original label instead of where you
            --        currently are with respect to the original label
                GT → (go original_label `on` (fromJust . leftChild)) current_label product_label
                LT → (go original_label `on` (fromJust . rightChild)) current_label product_label
-- }}}

instance Ord Label where -- {{{
    compare = compare `on` branchingFromLabel
-- }}}

instance Show Label where -- {{{
    show = fmap (\branch → case branch of {LeftBranch → 'L'; RightBranch → 'R'}) . branchingFromLabel
-- }}}

instance Monad m ⇒ MonadLabeled (LabeledT m) where -- {{{
    getLabel = LabeledT $ ask
-- }}}

instance MonadTrans LabeledTreeBuilderT where -- {{{
    lift = LabeledTreeBuilderT . lift . lift
-- }}}

instance MonadPlus m ⇒ MonadPlus (LabeledT m) where -- {{{
    mzero = LabeledT $ lift mzero
    LabeledT left `mplus` LabeledT right = LabeledT . ReaderT $
        \branch → (runReaderT left (leftChildLabel branch)) `mplus` (runReaderT right (rightChildLabel branch))
-- }}}

instance MonadVisitableTrans m ⇒ MonadVisitableTrans (LabeledT m) where -- {{{
    type NestedMonadInVisitor (LabeledT m) = NestedMonadInVisitor m
    runAndCache = LabeledT . lift . runAndCache
    runAndCacheGuard = LabeledT . lift . runAndCacheGuard
    runAndCacheMaybe = LabeledT . lift . runAndCacheMaybe
-- }}}

instance Monad m ⇒ MonadVisitableTrans (LabeledTreeBuilderT m) where -- {{{
    type NestedMonadInVisitor (LabeledTreeBuilderT m) = m
    runAndCache = LabeledTreeBuilderT . runAndCache
    runAndCacheGuard = LabeledTreeBuilderT . runAndCacheGuard
    runAndCacheMaybe = LabeledTreeBuilderT . runAndCacheMaybe
-- }}}

instance MonadPlus m ⇒ Monoid (LabeledT m α) where -- {{{
    mempty = mzero
    mappend = mplus
-- }}}

-- }}}

-- Functions {{{

applyCheckpointCursorToLabel :: CheckpointCursor → Label → Label -- {{{
applyCheckpointCursorToLabel cursor =
    case viewl cursor of
        EmptyL → id
        step :< rest →
            applyCheckpointCursorToLabel rest
            .
            case step of
                CacheCheckpointD _ → id
                ChoiceCheckpointD active_branch _ → labelTransformerForBranch active_branch
-- }}}

applyContextToLabel :: Context m α → Label → Label -- {{{
applyContextToLabel context =
    case viewl context of
        EmptyL → id
        step :< rest →
            applyContextToLabel rest
            .
            case step of
                CacheContextStep _ → id
                LeftBranchContextStep _ _ → leftChildLabel
                RightBranchContextStep → rightChildLabel
-- }}}

applyPathToLabel :: Path → Label → Label -- {{{
applyPathToLabel path =
    case viewl path of
        EmptyL → id
        step :< rest →
            applyPathToLabel rest
            .
            case step of
                ChoiceStep active_branch → labelTransformerForBranch active_branch
                CacheStep _ → id
-- }}}

branchingFromLabel :: Label → [Branch] -- {{{
branchingFromLabel = go root . unwrapLabel
  where
    go current_label original_label =
        case current_label `compare` original_label of
            EQ → []
            GT → LeftBranch:go (fromJust . leftChild $ current_label) original_label
            LT → RightBranch:go (fromJust . rightChild $ current_label) original_label
-- }}}

labelFromBranching :: Foldable t ⇒ t Branch → Label -- {{{
labelFromBranching = Fold.foldl' (flip labelTransformerForBranch) rootLabel
-- }}}

labelFromContext :: Context m α → Label -- {{{
labelFromContext = flip applyContextToLabel rootLabel
-- }}}

labelFromPath :: Path → Label -- {{{
labelFromPath = flip applyPathToLabel rootLabel
-- }}}

labelTransformerForBranch :: Branch → (Label → Label) -- {{{
labelTransformerForBranch LeftBranch = leftChildLabel
labelTransformerForBranch RightBranch = rightChildLabel
-- }}}

leftChildLabel :: Label → Label -- {{{
leftChildLabel = Label . fromJust . leftChild . unwrapLabel
-- }}}

normalizeLabeledTreeBuilder :: LabeledTreeBuilder α → TreeBuilder α -- {{{
normalizeLabeledTreeBuilder = runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

normalizeLabeledTreeBuilderT :: LabeledTreeBuilderT m α → TreeBuilderT m α -- {{{
normalizeLabeledTreeBuilderT = runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

rightChildLabel :: Label → Label -- {{{
rightChildLabel = Label . fromJust . rightChild . unwrapLabel
-- }}}

rootLabel :: Label -- {{{
rootLabel = Label root
-- }}}

runLabeledT :: LabeledT m α → m α -- {{{
runLabeledT = flip runReaderT rootLabel . unwrapLabeledT
-- }}}

runLabeledTreeBuilder :: Monoid α ⇒ LabeledTreeBuilder α → α -- {{{
runLabeledTreeBuilder = visitTree . runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

runLabeledTreeBuilderT :: (Monoid α,Monad m) ⇒ LabeledTreeBuilderT m α → m α -- {{{
runLabeledTreeBuilderT = visitTreeT . runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

runLabeledTreeBuilderTAndIgnoreResults :: Monad m ⇒ LabeledTreeBuilderT m α → m () -- {{{
runLabeledTreeBuilderTAndIgnoreResults = visitTreeTAndIgnoreResults . runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

visitTreeTWithLabelsAndGatherResults :: Monad m ⇒ TreeBuilderT m α → m [Solution α] -- {{{
visitTreeTWithLabelsAndGatherResults = visitTreeTWithStartingLabel rootLabel
-- }}}

visitTreeTWithStartingLabel :: Monad m ⇒ Label → TreeBuilderT m α → m [Solution α] -- {{{
visitTreeTWithStartingLabel label =
    viewT . unwrapTreeBuilderT >=> \view →
    case view of
        Return x → return [Solution label x]
        (Cache mx :>>= k) → mx >>= maybe (return []) (visitTreeTWithStartingLabel label . TreeBuilderT . k)
        (Choice left right :>>= k) →
            liftM2 (++)
                (visitTreeTWithStartingLabel (leftChildLabel label) $ left >>= TreeBuilderT . k)
                (visitTreeTWithStartingLabel (rightChildLabel label) $ right >>= TreeBuilderT . k)
        (Null :>>= _) → return []
-- }}}

visitTreeWithLabels :: TreeBuilder α → [Solution α] -- {{{
visitTreeWithLabels = runIdentity . visitTreeTWithLabelsAndGatherResults
-- }}}

visitTreeWithStartingLabel :: Label → TreeBuilder α → [Solution α] -- {{{
visitTreeWithStartingLabel = runIdentity .* visitTreeTWithStartingLabel
-- }}}

runLabeledTreeBuilderUntilFirst :: LabeledTreeBuilder α → Maybe α -- {{{
runLabeledTreeBuilderUntilFirst = visitTreeUntilFirst . runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

runLabeledTreeBuilderUntilFirstT :: Monad m ⇒ LabeledTreeBuilderT m α → m (Maybe α) -- {{{
runLabeledTreeBuilderUntilFirstT = visitTreeTUntilFirst . runLabeledT . unwrapLabeledTreeBuilderT
-- }}}

visitTreeTUntilFirstWithLabel :: Monad m ⇒ TreeBuilderT m α → m (Maybe (Solution α)) -- {{{
visitTreeTUntilFirstWithLabel = visitTreeTUntilFirstWithStartingLabel rootLabel
-- }}}

visitTreeTUntilFirstWithStartingLabel :: Monad m ⇒ Label → TreeBuilderT m α → m (Maybe (Solution α)) -- {{{
visitTreeTUntilFirstWithStartingLabel = go .* visitTreeTWithStartingLabel
  where
    go = liftM $ \solutions →
        case solutions of
            [] → Nothing
            (x:_) → Just x
-- }}}

visitTreeUntilFirstWithLabel :: TreeBuilder α → Maybe (Solution α) -- {{{
visitTreeUntilFirstWithLabel = runIdentity . visitTreeTUntilFirstWithLabel
-- }}}

visitTreeUntilFirstWithStartingLabel :: Label → TreeBuilder α → Maybe (Solution α) -- {{{
visitTreeUntilFirstWithStartingLabel = runIdentity .* visitTreeTUntilFirstWithStartingLabel
-- }}}

sendVisitorDownLabel :: Label → TreeBuilder α → TreeBuilder α -- {{{
sendVisitorDownLabel label = runIdentity . sendVisitorTDownLabel label
-- }}}

sendVisitorTDownLabel :: Monad m ⇒ Label → TreeBuilderT m α → m (TreeBuilderT m α) -- {{{
sendVisitorTDownLabel (Label label) = go root
  where
    go parent visitor
      | parent == label = return visitor
      | otherwise =
          (viewT . unwrapTreeBuilderT) visitor >>= \view → case view of
            Return _ → throw VisitorTerminatedBeforeEndOfWalk
            Null :>>= _ → throw VisitorTerminatedBeforeEndOfWalk
            Cache mx :>>= k → mx >>= maybe (throw VisitorTerminatedBeforeEndOfWalk) (go parent . TreeBuilderT . k)
            Choice left right :>>= k →
                if parent > label
                then
                    go
                        (fromJust . leftChild $ parent)
                        (left >>= TreeBuilderT . k)
                else
                    go
                        (fromJust . rightChild $ parent)
                        (right >>= TreeBuilderT . k)
-- }}}

solutionsToMap :: Foldable t ⇒ t (Solution α) → Map Label α -- {{{
solutionsToMap = Fold.foldl' (flip $ \(Solution label solution) → Map.insert label solution) Map.empty
-- }}}

-- }}}
