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
    getLabel :: m VisitorLabel
-- }}}

-- Types {{{

newtype LabeledT m α = LabeledT { unwrapLabeledT :: ReaderT VisitorLabel m α }
    deriving (Applicative,Functor,Monad,MonadIO,MonadTrans)
newtype LabeledVisitorT m α = LabeledVisitorT { unwrapLabeledVisitorT :: LabeledT (TreeBuilderT m) α }
    deriving (Alternative,Applicative,Functor,Monad,MonadIO,MonadLabeled,MonadPlus,Monoid)
type LabeledVisitorIO = LabeledVisitorT IO
type LabeledVisitor = LabeledVisitorT Identity

newtype VisitorLabel = VisitorLabel { unwrapVisitorLabel :: SequentialIndex } deriving (Eq)

data Solution α = Solution
    {   solutionLabel :: VisitorLabel
    ,   solutionResult :: α
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
        go original_label current_label product_label =
            case current_label `compare` original_label of
                EQ → product_label
            -- Note:  the following is counter-intuitive, but it makes sense if you think of it as
            --        being where you need to go to get to the original label instead of where you
            --        currently are with respect to the original label
                GT → (go original_label `on` (fromJust . leftChild)) current_label product_label
                LT → (go original_label `on` (fromJust . rightChild)) current_label product_label
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

instance MonadVisitorTrans m ⇒ MonadVisitorTrans (LabeledT m) where -- {{{
    type NestedMonadInVisitor (LabeledT m) = NestedMonadInVisitor m
    runAndCache = LabeledT . lift . runAndCache
    runAndCacheGuard = LabeledT . lift . runAndCacheGuard
    runAndCacheMaybe = LabeledT . lift . runAndCacheMaybe
-- }}}

instance Monad m ⇒ MonadVisitorTrans (LabeledVisitorT m) where -- {{{
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

applyContextToLabel :: Context m α → VisitorLabel → VisitorLabel -- {{{
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

applyPathToLabel :: Path → VisitorLabel → VisitorLabel -- {{{
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

branchingFromLabel :: VisitorLabel → [Branch] -- {{{
branchingFromLabel = go root . unwrapVisitorLabel
  where
    go current_label original_label =
        case current_label `compare` original_label of
            EQ → []
            GT → LeftBranch:go (fromJust . leftChild $ current_label) original_label
            LT → RightBranch:go (fromJust . rightChild $ current_label) original_label
-- }}}

labelFromBranching :: Foldable t ⇒ t Branch → VisitorLabel -- {{{
labelFromBranching = Fold.foldl' (flip labelTransformerForBranch) rootLabel
-- }}}

labelFromContext :: Context m α → VisitorLabel -- {{{
labelFromContext = flip applyContextToLabel rootLabel
-- }}}

labelFromPath :: Path → VisitorLabel -- {{{
labelFromPath = flip applyPathToLabel rootLabel
-- }}}

labelTransformerForBranch :: Branch → (VisitorLabel → VisitorLabel) -- {{{
labelTransformerForBranch LeftBranch = leftChildLabel
labelTransformerForBranch RightBranch = rightChildLabel
-- }}}

leftChildLabel :: VisitorLabel → VisitorLabel -- {{{
leftChildLabel = VisitorLabel . fromJust . leftChild . unwrapVisitorLabel
-- }}}

normalizeLabeledVisitor :: LabeledVisitor α → TreeBuilder α -- {{{
normalizeLabeledVisitor = runLabeledT . unwrapLabeledVisitorT
-- }}}

normalizeLabeledVisitorT :: LabeledVisitorT m α → TreeBuilderT m α -- {{{
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
runLabeledVisitor = visitTree . runLabeledT . unwrapLabeledVisitorT
-- }}}

runLabeledVisitorT :: (Monoid α,Monad m) ⇒ LabeledVisitorT m α → m α -- {{{
runLabeledVisitorT = visitTreeT . runLabeledT . unwrapLabeledVisitorT
-- }}}

runLabeledVisitorTAndIgnoreResults :: Monad m ⇒ LabeledVisitorT m α → m () -- {{{
runLabeledVisitorTAndIgnoreResults = visitTreeTAndIgnoreResults . runLabeledT . unwrapLabeledVisitorT
-- }}}

visitTreeTWithLabelsAndGatherResults :: Monad m ⇒ TreeBuilderT m α → m [Solution α] -- {{{
visitTreeTWithLabelsAndGatherResults = visitTreeTWithStartingLabel rootLabel
-- }}}

visitTreeTWithStartingLabel :: Monad m ⇒ VisitorLabel → TreeBuilderT m α → m [Solution α] -- {{{
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

visitTreeWithStartingLabel :: VisitorLabel → TreeBuilder α → [Solution α] -- {{{
visitTreeWithStartingLabel = runIdentity .* visitTreeTWithStartingLabel
-- }}}

runLabeledVisitorUntilFirst :: LabeledVisitor α → Maybe α -- {{{
runLabeledVisitorUntilFirst = visitTreeUntilFirst . runLabeledT . unwrapLabeledVisitorT
-- }}}

runLabeledVisitorUntilFirstT :: Monad m ⇒ LabeledVisitorT m α → m (Maybe α) -- {{{
runLabeledVisitorUntilFirstT = visitTreeTUntilFirst . runLabeledT . unwrapLabeledVisitorT
-- }}}

visitTreeTUntilFirstWithLabel :: Monad m ⇒ TreeBuilderT m α → m (Maybe (Solution α)) -- {{{
visitTreeTUntilFirstWithLabel = visitTreeTUntilFirstWithStartingLabel rootLabel
-- }}}

visitTreeTUntilFirstWithStartingLabel :: Monad m ⇒ VisitorLabel → TreeBuilderT m α → m (Maybe (Solution α)) -- {{{
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

visitTreeUntilFirstWithStartingLabel :: VisitorLabel → TreeBuilder α → Maybe (Solution α) -- {{{
visitTreeUntilFirstWithStartingLabel = runIdentity .* visitTreeTUntilFirstWithStartingLabel
-- }}}

sendVisitorDownLabel :: VisitorLabel → TreeBuilder α → TreeBuilder α -- {{{
sendVisitorDownLabel label = runIdentity . sendVisitorTDownLabel label
-- }}}

sendVisitorTDownLabel :: Monad m ⇒ VisitorLabel → TreeBuilderT m α → m (TreeBuilderT m α) -- {{{
sendVisitorTDownLabel (VisitorLabel label) = go root
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

solutionsToMap :: Foldable t ⇒ t (Solution α) → Map VisitorLabel α -- {{{
solutionsToMap = Fold.foldl' (flip $ \(Solution label solution) → Map.insert label solution) Map.empty
-- }}}

-- }}}
