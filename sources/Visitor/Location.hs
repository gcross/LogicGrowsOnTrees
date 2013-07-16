{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains infrastructure for working with 'Location's, which
    indicate a location within a tree but, unlike 'Path', without the cached
    values.
 -}
module Visitor.Location
    (
    -- * Type-classes
      MonadLocatable(..)
    -- * Types
    , Location(..)
    , Solution(..)
    , LocatableT(..)
    , LocatableTree(..)
    , LocatableTreeIO(..)
    , LocatableTreeT(..)
    -- * Utility functions
    , applyCheckpointCursorToLocation
    , applyContextToLocation
    , applyPathToLocation
    , branchingFromLocation
    , labelFromBranching
    , labelFromContext
    , labelFromPath
    , leftBranchOf
    , locationTransformerForBranchChoice
    , normalizeLocatableTree
    , normalizeLocatableTreeT
    , rightBranchOf
    , rootLocation
    , runLocatableT
    , sendTreeDownLocation
    , sendTreeTDownLocation
    , solutionsToMap
    -- * Exploration functions
    , exploreLocatableTree
    , exploreLocatableTreeT
    , exploreLocatableTreeTAndIgnoreResults
    , exploreTreeWithLocations
    , exploreTreeTWithLocations
    , exploreTreeWithLocationsStartingAt
    , exploreTreeTWithLocationsStartingAt
    , exploreLocatableTreeUntilFirst
    , exploreLocatableTreeUntilFirstT
    , exploreTreeUntilFirstWithLocation
    , exploreTreeTUntilFirstWithLocation
    , exploreTreeUntilFirstWithLocationStartingAt
    , exploreTreeTUntilFirstWithLocationStartingAt
    ) where


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

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{-| The class 'MonadLocatable' allows you to get your current location. -}
class MonadPlus m ⇒ MonadLocatable m where
    getLocation :: m Location

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

{-| A 'Location' identifies a location in a tree;  unlike 'Path' it only
    contains information about the list of branches that have been taken, and
    not information about the cached values encounted along the way.
 -}
newtype Location = Location { unwrapLocation :: SequentialIndex } deriving (Eq)

{-| A 'Solution' is a result tagged with the location of the leaf at which it
    was found.
 -}
data Solution α = Solution
    {   solutionLocation :: Location
    ,   solutionResult :: α
    } deriving (Eq,Ord,Show)

{-| The 'Monoid' instance constructs a location that is the result of appending
    the path in the second argument to the path in the first argument.
 -}
instance Monoid Location where
    mempty = rootLocation
    xl@(Location x) `mappend` yl@(Location y)
      | x == root = yl
      | y == root = xl
      | otherwise = Location $ go y root x
      where
        go original_label current_label product_label =
            case current_label `compare` original_label of
                EQ → product_label
            -- Note:  the following is counter-intuitive, but it makes sense if you think of it as
            --        being where you need to go to get to the original label instead of where you
            --        currently are with respect to the original label
                GT → (go original_label `on` (fromJust . leftChild)) current_label product_label
                LT → (go original_label `on` (fromJust . rightChild)) current_label product_label

{-| The 'Ord' instance performs the comparison using the list of branches in the
    path defined by the location, which is obtained using the function
    'branchingFromLocation'.
 -}
instance Ord Location where
    compare = compare `on` branchingFromLocation

instance Show Location where
    show = fmap (\branch → case branch of {LeftBranch → 'L'; RightBranch → 'R'}) . branchingFromLocation

{-| 'LocatableT' is a monad transformer that allows you to take any MonadPlus
    and add to it the ability to tell where you are in the tree created by the
    'mplus's.
 -}
newtype LocatableT m α = LocatableT { unwrapLocatableT :: ReaderT Location m α }
    deriving (Applicative,Functor,Monad,MonadIO,MonadTrans)

instance (Alternative m, Monad m) ⇒ Alternative (LocatableT m) where
    empty = LocatableT $ lift empty
    LocatableT left <|> LocatableT right = LocatableT . ReaderT $
        \branch → (runReaderT left (leftBranchOf branch)) <|> (runReaderT right (rightBranchOf branch))

instance MonadPlus m ⇒ MonadLocatable (LocatableT m) where
    getLocation = LocatableT $ ask

instance MonadPlus m ⇒ MonadPlus (LocatableT m) where
    mzero = LocatableT $ lift mzero
    LocatableT left `mplus` LocatableT right = LocatableT . ReaderT $
        \branch → (runReaderT left (leftBranchOf branch)) `mplus` (runReaderT right (rightBranchOf branch))

instance MonadExplorableTrans m ⇒ MonadExplorableTrans (LocatableT m) where
    type NestedMonad (LocatableT m) = NestedMonad m
    runAndCache = LocatableT . lift . runAndCache
    runAndCacheGuard = LocatableT . lift . runAndCacheGuard
    runAndCacheMaybe = LocatableT . lift . runAndCacheMaybe

instance MonadPlus m ⇒ Monoid (LocatableT m α) where
    mempty = mzero
    mappend = mplus

{-| A 'Tree' augmented with the ability to get the current location -}
type LocatableTree = LocatableTreeT Identity

{-| Like 'LocatableTree', but running in the IO monad. -}
type LocatableTreeIO = LocatableTreeT IO

{-| Like 'LocatableTree', but running in an arbitrary monad. -}
newtype LocatableTreeT m α = LocatableTreeT { unwrapLocatableTreeT :: LocatableT (TreeT m) α }
    deriving (Alternative,Applicative,Functor,Monad,MonadIO,MonadLocatable,MonadPlus,Monoid)

instance MonadTrans LocatableTreeT where
    lift = LocatableTreeT . lift . lift

instance Monad m ⇒ MonadExplorableTrans (LocatableTreeT m) where
    type NestedMonad (LocatableTreeT m) = m
    runAndCache = LocatableTreeT . runAndCache
    runAndCacheGuard = LocatableTreeT . runAndCacheGuard
    runAndCacheMaybe = LocatableTreeT . runAndCacheMaybe

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

------------------------------ Utility functions -------------------------------

{-| Append the path indicated by a checkpoint cursor to a location's path. -}
applyCheckpointCursorToLocation ::
    CheckpointCursor {-^ a path within the subtree -} →
    Location {-^ the location of the subtree -} →
    Location {-^ the location within the subtree obtained by following the path
                 indicated by the checkpoint cursor
              -}
applyCheckpointCursorToLocation cursor =
    case viewl cursor of
        EmptyL → id
        step :< rest →
            applyCheckpointCursorToLocation rest
            .
            case step of
                CachePointD _ → id
                ChoicePointD active_branch _ → locationTransformerForBranchChoice active_branch

{-| Append the path indicated by a context to a location's path. -}
applyContextToLocation ::
    Context m α {-^ the path within the subtree -} →
    Location {-^ the location of the subtree -} →
    Location {-^ the location within the subtree obtained by following the path
                 indicated by the context
              -}
applyContextToLocation context =
    case viewl context of
        EmptyL → id
        step :< rest →
            applyContextToLocation rest
            .
            case step of
                CacheContextStep _ → id
                LeftBranchContextStep _ _ → leftBranchOf
                RightBranchContextStep → rightBranchOf

{-| Append a path to a location's path. -}
applyPathToLocation ::
    Path {-^ a path within the subtree -} →
    Location {-^ the location of the subtree -} →
    Location {-^ the location within the subtree obtained by following the given path -}
applyPathToLocation path =
    case viewl path of
        EmptyL → id
        step :< rest →
            applyPathToLocation rest
            .
            case step of
                ChoiceStep active_branch → locationTransformerForBranchChoice active_branch
                CacheStep _ → id

{-| Converts a location to a list of branch choices. -}
branchingFromLocation :: Location → [BranchChoice]
branchingFromLocation = go root . unwrapLocation
  where
    go current_label original_label =
        case current_label `compare` original_label of
            EQ → []
            GT → LeftBranch:go (fromJust . leftChild $ current_label) original_label
            LT → RightBranch:go (fromJust . rightChild $ current_label) original_label

{-| Converts a list (or other 'Foldable') of branch choices to a location. -}
labelFromBranching :: Foldable t ⇒ t BranchChoice → Location
labelFromBranching = Fold.foldl' (flip locationTransformerForBranchChoice) rootLocation

{-| Contructs a 'Location' representing the location within the tree indicated by the 'Context'. -}
labelFromContext :: Context m α → Location
labelFromContext = flip applyContextToLocation rootLocation

{-| Contructs a 'Location' representing the location within the tree indicated by the 'Path'. -}
labelFromPath :: Path → Location
labelFromPath = flip applyPathToLocation rootLocation

{-| Returns the 'Location' at the left branch of the given location. -}
leftBranchOf :: Location → Location
leftBranchOf = Location . fromJust . leftChild . unwrapLocation

{-| Convenience function takes a branch choice and returns a location
    transformer that appends the branch choice to the given location.
 -}
locationTransformerForBranchChoice :: BranchChoice → (Location → Location)
locationTransformerForBranchChoice LeftBranch = leftBranchOf
locationTransformerForBranchChoice RightBranch = rightBranchOf

{-| Converts a 'LocatableTree' to a 'Tree'. -}
normalizeLocatableTree :: LocatableTree α → Tree α
normalizeLocatableTree = runLocatableT . unwrapLocatableTreeT

{-| Converts a 'LocatableTreeT' to a 'TreeT'. -}
normalizeLocatableTreeT :: LocatableTreeT m α → TreeT m α
normalizeLocatableTreeT = runLocatableT . unwrapLocatableTreeT

{-| Returns the 'Location' at the right branch of the given location. -}
rightBranchOf :: Location → Location
rightBranchOf = Location . fromJust . rightChild . unwrapLocation

{-| The location at the root of the tree. -}
rootLocation :: Location
rootLocation = Location root

{-| Runs a 'LocatableT' to obtain the nested monad. -}
runLocatableT :: LocatableT m α → m α
runLocatableT = flip runReaderT rootLocation . unwrapLocatableT

{-| Guides a 'Tree' guiding it to the subtree at the given 'Location'.
    This function is analagous to 'Visitor.Path.sendTreeDownPath', and
    shares the same caveats.
 -}
sendTreeDownLocation :: Location → Tree α → Tree α
sendTreeDownLocation label = runIdentity . sendTreeTDownLocation label

{-| Like 'sendTreeDownLocation', but for impure trees. -}
sendTreeTDownLocation :: Monad m ⇒ Location → TreeT m α → m (TreeT m α)
sendTreeTDownLocation (Location label) = go root
  where
    go parent tree
      | parent == label = return tree
      | otherwise =
          (viewT . unwrapTreeT) tree >>= \view → case view of
            Return _ → throw TreeEndedBeforeEndOfWalk
            Null :>>= _ → throw TreeEndedBeforeEndOfWalk
            Cache mx :>>= k → mx >>= maybe (throw TreeEndedBeforeEndOfWalk) (go parent . TreeT . k)
            Choice left right :>>= k →
                if parent > label
                then
                    go
                        (fromJust . leftChild $ parent)
                        (left >>= TreeT . k)
                else
                    go
                        (fromJust . rightChild $ parent)
                        (right >>= TreeT . k)

{-| Converts a list (or other 'Foldable') of solutions to a 'Map' from
    'Location's to results.
 -}
solutionsToMap :: Foldable t ⇒ t (Solution α) → Map Location α
solutionsToMap = Fold.foldl' (flip $ \(Solution label solution) → Map.insert label solution) Map.empty

------------------------------ Exploration functions -------------------------------

{-| Explore all the nodes in a LocatableTree and sum over all the results in the
    leaves.
 -}
exploreLocatableTree :: Monoid α ⇒ LocatableTree α → α
exploreLocatableTree = exploreTree . runLocatableT . unwrapLocatableTreeT

{-| Same as 'exploreLocatableTree', but for an impure tree. -}
exploreLocatableTreeT :: (Monoid α,Monad m) ⇒ LocatableTreeT m α → m α
exploreLocatableTreeT = exploreTreeT . runLocatableT . unwrapLocatableTreeT

{-| Same as 'exploreLocatableTree', but the results are discarded so the tree is
    only explored for its side-effects.
 -}
exploreLocatableTreeTAndIgnoreResults :: Monad m ⇒ LocatableTreeT m α → m ()
exploreLocatableTreeTAndIgnoreResults = exploreTreeTAndIgnoreResults . runLocatableT . unwrapLocatableTreeT

{-| Explores all of the nodes of a tree, returning a list of solutions each
    tagged with the location at which it was found.
 -}
exploreTreeWithLocations :: Tree α → [Solution α]
exploreTreeWithLocations = runIdentity . exploreTreeTWithLocations

{-| Like 'exploreTreeWithLocations' but for an impure tree. -}
exploreTreeTWithLocations :: Monad m ⇒ TreeT m α → m [Solution α]
exploreTreeTWithLocations = exploreTreeTWithLocationsStartingAt rootLocation

{-| Like 'exploreTreeWithLocations', but for a subtree whose location is given by
    the first argument;  the solutions are labeled by the /absolute/ location
    within the full tree (as opposed to their relative location within the
    subtree).
 -}
exploreTreeWithLocationsStartingAt :: Location → Tree α → [Solution α]
exploreTreeWithLocationsStartingAt = runIdentity .* exploreTreeTWithLocationsStartingAt

{-| Like 'exploreTreeWithLocationsStartingAt' but for an impure trees. -}
exploreTreeTWithLocationsStartingAt :: Monad m ⇒ Location → TreeT m α → m [Solution α]
exploreTreeTWithLocationsStartingAt label =
    viewT . unwrapTreeT >=> \view →
    case view of
        Return x → return [Solution label x]
        (Cache mx :>>= k) → mx >>= maybe (return []) (exploreTreeTWithLocationsStartingAt label . TreeT . k)
        (Choice left right :>>= k) →
            liftM2 (++)
                (exploreTreeTWithLocationsStartingAt (leftBranchOf label) $ left >>= TreeT . k)
                (exploreTreeTWithLocationsStartingAt (rightBranchOf label) $ right >>= TreeT . k)
        (Null :>>= _) → return []

{-| Explores all the nodes in a locatable tree until a result (i.e., a leaf) has
    been found; if a result has been found then it is returned wrapped in
    'Just', otherwise 'Nothing' is returned.
 -}
exploreLocatableTreeUntilFirst :: LocatableTree α → Maybe α
exploreLocatableTreeUntilFirst = exploreTreeUntilFirst . runLocatableT . unwrapLocatableTreeT

{-| Like 'exploreLocatableTreeUntilFirst' but for an impure tree. -}
exploreLocatableTreeUntilFirstT :: Monad m ⇒ LocatableTreeT m α → m (Maybe α)
exploreLocatableTreeUntilFirstT = exploreTreeTUntilFirst . runLocatableT . unwrapLocatableTreeT

{-| Explores all the nodes in a tree until a result (i.e., a leaf) has been found;
    if a result has been found then it is returned tagged with the location at
    which it was found and wrapped in 'Just', otherwise'Nothing' is returned.
 -}
exploreTreeUntilFirstWithLocation :: Tree α → Maybe (Solution α)
exploreTreeUntilFirstWithLocation = runIdentity . exploreTreeTUntilFirstWithLocation

{-| Like 'exploreTreeUntilFirstWithLocation' but for an impure tree. -}
exploreTreeTUntilFirstWithLocation :: Monad m ⇒ TreeT m α → m (Maybe (Solution α))
exploreTreeTUntilFirstWithLocation = exploreTreeTUntilFirstWithLocationStartingAt rootLocation

{-| Like 'exploreTreeUntilFirstWithLocation', but for a subtree whose location is
    given by the first argument; the solution (if present) is labeled by the
    /absolute/ location within the full tree (as opposed to its relative
    location within the subtree).
 -}
exploreTreeUntilFirstWithLocationStartingAt :: Location → Tree α → Maybe (Solution α)
exploreTreeUntilFirstWithLocationStartingAt = runIdentity .* exploreTreeTUntilFirstWithLocationStartingAt

{-| Like 'exploreTreeUntilFirstWithLocationStartingAt' but for an impure tree. -}
exploreTreeTUntilFirstWithLocationStartingAt :: Monad m ⇒ Location → TreeT m α → m (Maybe (Solution α))
exploreTreeTUntilFirstWithLocationStartingAt = go .* exploreTreeTWithLocationsStartingAt
  where
    go = liftM $ \solutions →
        case solutions of
            [] → Nothing
            (x:_) → Just x
