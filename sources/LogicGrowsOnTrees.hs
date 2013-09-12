{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| Basic functionality for building and exploring trees. -}
module LogicGrowsOnTrees
    (
    -- * Tree types
    -- $types
      Tree
    , TreeIO
    , TreeT(..)
    -- * Explorable class features
    -- $type-classes
    , MonadExplorable(..)
    , MonadExplorableTrans(..)
    -- * Functions
    -- $functions

    -- ** ...that explore trees
    -- $runners
    , exploreTree
    , exploreTreeT
    , exploreTreeTAndIgnoreResults
    , exploreTreeUntilFirst
    , exploreTreeTUntilFirst
    , exploreTreeUntilFound
    , exploreTreeTUntilFound
    -- ** ...that help building trees
    -- $builders
    , allFrom
    , between
    -- ** ...that transform trees
    , endowTree
    -- * Implementation
    -- $implementation
    , TreeTInstruction(..)
    , TreeInstruction
    ) where

import Control.Applicative (Alternative(..),Applicative(..))
import Control.Monad (MonadPlus(..),(>=>),liftM,liftM2,msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Operational (ProgramT,ProgramViewT(..),singleton,view,viewT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)

import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold

import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Maybe (isJust)
import Data.Monoid ((<>),Monoid(..))
import Data.Serialize (Serialize(),encode)

--------------------------------------------------------------------------------
------------------------------------- Types ------------------------------------
--------------------------------------------------------------------------------

{- $types
The following are the tree types that are accepted by most of the functions in
this package. You do not need to know the details of their definitions unless
you intend to write your own custom routines for running and transforming trees,
in which case the relevant information is at the bottom of this page in the
Implementation section.

There is one type of pure tree and two types of impure trees. In general, your
tree should nearly always be pure if you are planning to make use of
checkpointing or parallel exploring, as parts of the tree may be explored
multiple times, some parts may not be run at all on a given processor, and
whenever a leaf is hit there will be a jump to a higher node, so if your tree is
impure then the result needs to not depend on how the tree is explored; an
example of an acceptable use of an inner monad is when you want to memoize a
pure function using a stateful monad.

If you need something like state in your tree, then you should consider
nesting the tree monad in the state monad rather than vice-versa,
because this will do things like automatically erasing the change in state that
happened between an inner node and a leaf when the tree jumps back up
from the leaf to an inner node, which will usually be what you want.
-}

{-| A pure tree, which is what you should normally be using. -}
type Tree = TreeT Identity

{-| A tree running in the I/O monad, which you should only be using for doing
    things like reading data from an external file or database that will be
    constant for the entire run.
-}
type TreeIO = TreeT IO

{-| A tree run in an arbitrary monad. -}
newtype TreeT m α = TreeT { unwrapTreeT :: ProgramT (TreeTInstruction m) m α }
    deriving (Applicative,Functor,Monad,MonadIO)

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{- $type-classes

'Tree's are instances of 'MonadExplorable' and 'MonadExplorableTrans',
which are both subclasses of 'MonadPlus'. The additional functionality offered
by these type-classes is the ability to cache results so that a computation does
not need to be repeated when a node is explored a second time, which can happen
either when resuming from a checkpoint or when a workload has been stolen by
another processor, as the first step is to retrace the path through the tree
that leads to the stolen workload.

These features could have been provided as functions, but there are two reasons
why they were subsumed into type-classes: first, because one might want to
add another layer above the 'Tree' monad transformers in the monad stack
(as is the case in "LogicGrowsOnTrees.Location"), and second, because one might want
to run a tree using a simpler monad such as List for testing purposes.

NOTE:  Caching a computation takes space in the 'Checkpoint', so it is something
       you should only do when the result is relatively small and the
       computation is very expensive and is high enough in the search tree that
       it is likely to be repeated often.  If the calculation is low enough in
       the search tree that it is unlikely to be repeated, is cheap enough so
       that repeating it is not a big deal, or produces a result with an
       incredibly large memory footprint, then you are probably better off not
       caching the result.
 -}

{-| The 'MonadExplorable' class provides caching functionality when exploring a
    tree, as well as a way to give a worker a chance to process any pending
    requests; at minimum 'cacheMaybe' needs to be defined.
 -}
class MonadPlus m ⇒ MonadExplorable m where
    {-| Cache a value in case we explore this node again. -}
    cache :: Serialize x ⇒ x → m x
    cache = cacheMaybe . Just

    {-| This does the same thing as 'guard' but it caches the result. -}
    cacheGuard :: Bool → m ()
    cacheGuard = cacheMaybe . (\x → if x then Just () else Nothing)

    {-| This function is a combination of the previous two;  it performs a
        computation which might fail by returning 'Nothing', and if that happens
        it then backtracks; if it passes then the result is cached and returned.

        Note that the previous two methods are essentially specializations of
        this method.
     -}
    cacheMaybe :: Serialize x ⇒ Maybe x → m x

    {-| This function tells the worker to take a break to process any pending
        requests; it does nothing if we are not in a parallel setting.

        NOTE: You should normally never need to use this function, as requests
        are processed whenever a choice point, a cache point, mzero, or a leaf
        in the decision tree has been encountered. However, if you have noticed
        that workload steals are taking such a large amount of time that workers
        are spending too much time sitting idle while they wait for a workload,
        and you can trace this as being due to a computation that takes so much
        time that it almost never gives the worker a chance to process requests,
        then you can use this method to ensure that requests are given a chance
        to be processed.
     -}
    processPendingRequests :: m ()
    processPendingRequests = return ()

{-| This class is like 'MonadExplorable', but it is designed to work with monad
    stacks;  at minimum 'runAndCacheMaybe' needs to be defined.
 -}
class (MonadPlus m, Monad (NestedMonad m)) ⇒ MonadExplorableTrans m where
    {-| The next layer down in the monad transformer stack. -}
    type NestedMonad m :: * → *

    {-| Runs the given action in the nested monad and caches the result. -}
    runAndCache :: Serialize x ⇒ (NestedMonad m) x → m x
    runAndCache = runAndCacheMaybe . liftM Just

    {-| Runs the given action in the nested monad and then does the equivalent
        of feeding it into 'guard', caching the result.
     -}
    runAndCacheGuard :: (NestedMonad m) Bool → m ()
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)

    {-| Runs the given action in the nested monad;  if it returns 'Nothing',
        then it acts like 'mzero',  if it returns 'Just x', then it caches the
        result.
     -}
    runAndCacheMaybe :: Serialize x ⇒ (NestedMonad m) (Maybe x) → m x

--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------

{-| The 'Alternative' instance functions just like the 'MonadPlus' instance. -}
instance Monad m ⇒ Alternative (TreeT m) where
    empty = mzero
    (<|>) = mplus

{-| Two 'Tree's are equal if they have the same structure. -}
instance Eq α ⇒ Eq (Tree α) where
    (TreeT x) == (TreeT y) = e x y
      where
        e x y = case (view x, view y) of
            (Return x, Return y) → x == y
            (Null :>>= _, Null :>>= _) → True
            (Cache cx :>>= kx, Cache cy :>>= ky) →
                case (runIdentity cx, runIdentity cy) of
                    (Nothing, Nothing) → True
                    (Just x, Just y) → e (kx x) (ky y)
                    _ → False
            (Choice (TreeT ax) (TreeT bx) :>>= kx, Choice (TreeT ay) (TreeT by) :>>= ky) →
                e (ax >>= kx) (ay >>= ky) && e (bx >>= kx) (by >>= ky)
            (ProcessPendingRequests :>>= kx,ProcessPendingRequests :>>= ky) → e (kx ()) (ky ())
            _  → False

{-| For this type, 'mplus' creates a branch node with a choice between two
    subtrees and 'mzero' signifies failure which results in backtracking up the
    tree.
 -}
instance Monad m ⇒ MonadPlus (TreeT m) where
    mzero = TreeT . singleton $ Null
    left `mplus` right = TreeT . singleton $ Choice left right

{-| This instance performs no caching but is provided to make it easier to test
    running a tree using the List monad.
 -}
instance MonadExplorable [] where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a tree using the 'ListT' monad.
 -}
instance Monad m ⇒ MonadExplorable (ListT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadExplorable' instance, this instance does no caching. -}
instance Monad m ⇒ MonadExplorableTrans (ListT m) where
    type NestedMonad (ListT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a tree using the 'Maybe' monad.
 -}
instance MonadExplorable Maybe where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a tree using the 'MaybeT' monad.
 -}
instance Monad m ⇒ MonadExplorable (MaybeT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadExplorable' instance, this instance does no caching. -}
instance Monad m ⇒ MonadExplorableTrans (MaybeT m) where
    type NestedMonad (MaybeT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

instance Monad m ⇒ MonadExplorable (TreeT m) where
    cache = runAndCache . return
    cacheGuard = runAndCacheGuard . return
    cacheMaybe = runAndCacheMaybe . return
    processPendingRequests = TreeT . singleton $ ProcessPendingRequests

instance Monad m ⇒ MonadExplorableTrans (TreeT m) where
    type NestedMonad (TreeT m) = m
    runAndCache = runAndCacheMaybe . liftM Just
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)
    runAndCacheMaybe = TreeT . singleton . Cache

instance MonadTrans TreeT where
    lift = TreeT . lift

{-| The 'Monoid' instance acts like the 'MonadPlus' instance. -}
instance Monad m ⇒ Monoid (TreeT m α) where
    mempty = mzero
    mappend = mplus
    mconcat = msum

instance Show α ⇒ Show (Tree α) where
    show = s . unwrapTreeT
      where
        s x = case view x of
            Return x → show x
            Null :>>= _ → "<NULL> >>= (...)"
            ProcessPendingRequests :>>= k → "<PPR> >>= " ++ (s . k $ ())
            Cache c :>>= k →
                case runIdentity c of
                    Nothing → "NullCache"
                    Just x → "Cache[" ++ (show . encode $ x) ++ "] >>= " ++ (s (k x))
            Choice (TreeT a) (TreeT b) :>>= k → "(" ++ (s (a >>= k)) ++ ") | (" ++ (s (b >>= k)) ++ ")"


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{- $functions
There are three kinds of functions in this module: functions that explore trees
in various ways, functions that make it easier to build trees, and a function
that changes the base monad of a pure tree.
 -}

---------------------------------- Explorers -----------------------------------

{- $runners
The following functions all take a tree as input and produce the result of
exploring it as output. There are seven functions because there are two kinds of
trees --- pure and impure --- and three ways of exploring a tree --- exploring
everything and summing all results (i.e., in the leaves), exploring until the
first result (i.e., in a leaf) is encountered and immediately returning, and
gathering results (i.e., from the leaves) until they satisfy a condition and
then returning --- plus a seventh function that explores a tree only for the
side-effects.
 -}

{-| Explores all the nodes in a pure tree and sums over all the
    results in the leaves.
 -}
exploreTree ::
    Monoid α ⇒
    Tree α {-^ the (pure) tree to be explored -} →
    α {-^ the sum over all results -}
exploreTree v =
    case view (unwrapTreeT v) of
        Return !x → x
        Cache mx :>>= k → maybe mempty (exploreTree . TreeT . k) (runIdentity mx)
        Choice left right :>>= k →
            let !x = exploreTree $ left >>= TreeT . k
                !y = exploreTree $ right >>= TreeT . k
                !xy = mappend x y
            in xy
        Null :>>= _ → mempty
        ProcessPendingRequests :>>= k → exploreTree . TreeT . k $ ()
{-# INLINEABLE exploreTree #-}

{-| Explores all the nodes in an impure tree and sums over all the
    results in the leaves.
 -}
exploreTreeT ::
    (Monad m, Monoid α) ⇒
    TreeT m α {-^ the (impure) tree to be explored -} →
    m α {-^ the sum over all results -}
exploreTreeT = viewT . unwrapTreeT >=> \view →
    case view of
        Return !x → return x
        Cache mx :>>= k → mx >>= maybe (return mempty) (exploreTreeT . TreeT . k)
        Choice left right :>>= k →
            liftM2 (\(!x) (!y) → let !xy = mappend x y in xy)
                (exploreTreeT $ left >>= TreeT . k)
                (exploreTreeT $ right >>= TreeT . k)
        Null :>>= _ → return mempty
        ProcessPendingRequests :>>= k → exploreTreeT . TreeT . k $ ()
{-# SPECIALIZE exploreTreeT :: Monoid α ⇒ Tree α → Identity α #-}
{-# SPECIALIZE exploreTreeT :: Monoid α ⇒ TreeIO α → IO α #-}
{-# INLINEABLE exploreTreeT #-}

{-| Explores a tree for its side-effects, ignoring all results. -}
exploreTreeTAndIgnoreResults ::
    Monad m ⇒
    TreeT m α {-^ the (impure) tree to be explored -} →
    m ()
exploreTreeTAndIgnoreResults = viewT . unwrapTreeT >=> \view →
    case view of
        Return _ → return ()
        Cache mx :>>= k → mx >>= maybe (return ()) (exploreTreeTAndIgnoreResults . TreeT . k)
        Choice left right :>>= k → do
            exploreTreeTAndIgnoreResults $ left >>= TreeT . k
            exploreTreeTAndIgnoreResults $ right >>= TreeT . k
        Null :>>= _ → return ()
        ProcessPendingRequests :>>= k → exploreTreeTAndIgnoreResults . TreeT . k $ ()
{-# SPECIALIZE exploreTreeTAndIgnoreResults :: Tree α → Identity () #-}
{-# SPECIALIZE exploreTreeTAndIgnoreResults :: TreeIO α → IO () #-}
{-# INLINEABLE exploreTreeTAndIgnoreResults #-}

{-| Explores all the nodes in a tree until a result (i.e., a leaf) has been
    found; if a result has been found then it is returned wrapped in 'Just',
    otherwise 'Nothing' is returned.
 -}
exploreTreeUntilFirst ::
    Tree α {-^ the (pure) tree to be explored -} →
    Maybe α {-^ the first result found, if any -}
exploreTreeUntilFirst v =
    case view (unwrapTreeT v) of
        Return x → Just x
        Cache mx :>>= k → maybe Nothing (exploreTreeUntilFirst . TreeT . k) (runIdentity mx)
        Choice left right :>>= k →
            let x = exploreTreeUntilFirst $ left >>= TreeT . k
                y = exploreTreeUntilFirst $ right >>= TreeT . k
            in if isJust x then x else y
        Null :>>= _ → Nothing
        ProcessPendingRequests :>>= k → exploreTreeUntilFirst . TreeT . k $ ()
{-# INLINEABLE exploreTreeUntilFirst #-}

{-| Same as 'exploreTreeUntilFirst', but taking an impure tree instead
    of pure one.
 -}
exploreTreeTUntilFirst ::
    Monad m ⇒
    TreeT m α {-^ the (impure) tree to be explored -} →
    m (Maybe α) {-^ the first result found, if any -}
exploreTreeTUntilFirst = viewT . unwrapTreeT >=> \view →
    case view of
        Return !x → return (Just x)
        Cache mx :>>= k → mx >>= maybe (return Nothing) (exploreTreeTUntilFirst . TreeT . k)
        Choice left right :>>= k → do
            x ← exploreTreeTUntilFirst $ left >>= TreeT . k
            if isJust x
                then return x
                else exploreTreeTUntilFirst $ right >>= TreeT . k
        Null :>>= _ → return Nothing
        ProcessPendingRequests :>>= k → exploreTreeTUntilFirst . TreeT . k $ ()
{-# SPECIALIZE exploreTreeTUntilFirst :: Tree α → Identity (Maybe α) #-}
{-# SPECIALIZE exploreTreeTUntilFirst :: TreeIO α → IO (Maybe α) #-}
{-# INLINEABLE exploreTreeTUntilFirst #-}

{-| Explores all the nodes in a tree, summing all encountered results (i.e., in
    the leaves) until the current partial sum satisfies the condition provided
    by the first function. The returned value is a pair where the first
    component is all of the results that were found during the exploration and
    the second component is 'True' if the exploration terminated early due to
    the condition being met and 'False' otherwise.

    NOTE:  The condition function is assumed to have two properties: first, it
           is assumed to return 'False' for 'mempty', and second, it is assumed
           that if it returns 'True' for @x@ then it also returns 'True' for
           @mappend x y@ and @mappend y x@ for all values @y@.  The reason for
           this is that the condition function is used to indicate when enough
           results have been found, and so it should not be 'True' for 'mempty'
           as nothing has been found and if it is 'True' for @x@ then it should
           not be 'False' for the sum of @y@ with @x@ as this would mean that
           having /more/ than enough results is no longer having enough results.
 -}
exploreTreeUntilFound ::
    Monoid α ⇒
    (α → Bool) {-^ a function that determines when the desired results have been found -} →
    Tree α {-^ the (pure) tree to be explored -} →
    (α,Bool) {-^ the result of the exploration, which includes the results that
                 were found and a flag indicating if they matched the condition
                 function
              -}
exploreTreeUntilFound f v =
    case view (unwrapTreeT v) of
        Return x → (x,f x)
        Cache mx :>>= k →
            maybe (mempty,False) (exploreTreeUntilFound f . TreeT . k)
            $
            runIdentity mx
        Choice left right :>>= k →
            let x@(xr,xf) = exploreTreeUntilFound f $ left >>= TreeT . k
                (yr,yf) = exploreTreeUntilFound f $ right >>= TreeT . k
                zr = xr <> yr
            in if xf then x else (zr,yf || f zr)
        Null :>>= _ → (mempty,False)
        ProcessPendingRequests :>>= k → exploreTreeUntilFound f . TreeT . k $ ()

{-| Same as 'exploreTreeUntilFound', but taking an impure tree instead of
    a pure tree.
 -}
exploreTreeTUntilFound ::
    (Monad m, Monoid α) ⇒
    (α → Bool) {-^ a function that determines when the desired results have been
                   found; it is assumed that this function is 'False' for 'mempty'
                -} →
    TreeT m α {-^ the (impure) tree to be explored -} →
    m (α,Bool) {-^ the result of the exploration, which includes the results
                   that were found and a flag indicating if they matched the
                   condition function
                -}
exploreTreeTUntilFound f = viewT . unwrapTreeT >=> \view →
    case view of
        Return x → return (x,f x)
        Cache mx :>>= k →
            mx
            >>=
            maybe (return (mempty,False)) (exploreTreeTUntilFound f . TreeT . k)
        Choice left right :>>= k → do
            x@(xr,xf) ← exploreTreeTUntilFound f $ left >>= TreeT . k
            if xf
                then return x
                else do
                    (yr,yf) ← exploreTreeTUntilFound f $ right >>= TreeT . k
                    let zr = xr <> yr
                    return (zr,yf || f zr)
        Null :>>= _ → return (mempty,False)
        ProcessPendingRequests :>>= k → exploreTreeTUntilFound f . TreeT . k $ ()

---------------------------------- Builders ------------------------------------

{- $builders
The following functions all create a tree from various inputs.
 -}

{-| Returns a tree (or some other 'MonadPlus') with all of the results in the
    input list.
 -}
allFrom ::
    (Foldable t, Functor t, MonadPlus m) ⇒
    t α {-^ the list (or some other `Foldable`) of results to generate -} →
    m α {-^ a tree that generates the given list of results -}
allFrom = Fold.msum . fmap return
{-# INLINE allFrom #-}

{-| Returns an optimally balanced tree (or some other 'MonadPlus') that
    generates all of the elements in the given (inclusive) range; if the lower
    bound is greater than the upper bound it returns 'mzero'.
 -}
between ::
    (Enum n, MonadPlus m) ⇒
    n {-^ the (inclusive) lower bound of the range -} →
    n {-^ the (inclusive) upper bound of the range -} →
    m n {-^ a tree (or other 'MonadPlus') that generates all the results in the range -}
between x y =
    if a > b
        then mzero
        else go a b
  where
    a = fromEnum x
    b = fromEnum y

    go a b | a == b    = return (toEnum a)
    go a b | otherwise = go a (a+d) `mplus` go (a+d+1) b
      where
        d = (b-a) `div` 2
{-# INLINE between #-}

-------------------------------- Transformers ----------------------------------

{-| This function lets you take a pure tree and transform it into a
    tree with an arbitrary base monad.
 -}
endowTree ::
    Monad m ⇒
    Tree α {-^ the pure tree to transformed into an impure tree -} →
    TreeT m α {-^ the resulting impure tree -}
endowTree tree =
    case view . unwrapTreeT $ tree of
        Return x → return x
        Cache mx :>>= k →
            cacheMaybe (runIdentity mx) >>= endowTree . TreeT . k
        Choice left right :>>= k →
            mplus
                (endowTree left >>= endowTree . TreeT . k)
                (endowTree right >>= endowTree . TreeT . k)
        Null :>>= _ → mzero
        ProcessPendingRequests :>>= k → endowTree . TreeT . k $ ()

--------------------------------------------------------------------------------
------------------------------- Implementation ---------------------------------
--------------------------------------------------------------------------------

{- $implementation
The implementation of the 'Tree' types uses the approach described in \"The
Operational Monad Tutorial\", published in
<http://themonadreader.wordpress.com/ Issue 15 of The Monad.Reader>;
specifically it uses the @operational@ package. The idea is that a list of
instructions are provided in 'TreeTInstruction', and then the operational monad
does all the heavy lifting of turning them into a monad.
 -}

{-| The core of the implementation of 'Tree' is mostly contained in this
    type, which provides a list of primitive instructions for trees:
    'Cache', which caches a value, 'Choice', which signals a branch with two
    choices, 'Null', which indicates that there are no more results, and
    'ProcessPendingRequests', which signals that a break should be taken from
    exploration to process any pending requests (only meant to be used in
    exceptional cases).
 -}
data TreeTInstruction m α where
    Cache :: Serialize α ⇒ m (Maybe α) → TreeTInstruction m α
    Choice :: TreeT m α → TreeT m α → TreeTInstruction m α
    Null :: TreeTInstruction m α
    ProcessPendingRequests :: TreeTInstruction m ()

{-| This is just a convenient alias for working with pure trees. -}
type TreeInstruction = TreeTInstruction Identity
