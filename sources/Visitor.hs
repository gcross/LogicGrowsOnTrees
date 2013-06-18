{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| Basic functionality for building and visiting trees. -}
module Visitor
    (
    -- * TreeGenerator types
    -- $types
      TreeGenerator
    , TreeGeneratorIO
    , TreeGeneratorT(..)
    -- * Visitable class features
    -- $type-classes
    , MonadVisitable(..)
    , MonadVisitableTrans(..)
    -- * Functions
    -- $functions

    -- ** ...that visit trees
    -- $runners
    , visitTree
    , visitTreeT
    , visitTreeTAndIgnoreResults
    , visitTreeUntilFirst
    , visitTreeTUntilFirst
    , visitTreeUntilFound
    , visitTreeTUntilFound
    -- ** ...that help building trees
    -- $builders
    , allFrom
    , allFromBalanced
    , allFromBalancedBottomUp
    , between
    , msumBalanced
    , msumBalancedBottomUp
    -- ** ...that transform tree builders
    , endowTreeGenerator
    -- * Implementation
    , TreeGeneratorTInstruction(..)
    , TreeGeneratorInstruction
    ) where

import Control.Applicative (Alternative(..),Applicative(..))
import Control.Monad (MonadPlus(..),(>=>),guard,liftM,liftM2,msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Operational (ProgramT,ProgramViewT(..),singleton,view,viewT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)

import Data.Array ((!),listArray)
import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Maybe (isJust)
import Data.Monoid ((<>),Monoid(..))
import Data.Serialize (Serialize(),encode)

import Visitor.Utils.MonadPlusForest

--------------------------------------------------------------------------------
------------------------------------- Types ------------------------------------
--------------------------------------------------------------------------------

{- $types
The following are the tree builder types that are accepted by most of he
functions in this package.  You do not need to know the details of their
definitions unless you intend to write your own custom routines for running and
transforming tree builders, in which case the relevant information is at the bottom
of this page in the Implementation section.

There is one type of pure tree builder and two types of impure tree builders.
In general, your tree builder should nearly always be pure if you are planning
to make use of checkpointing or parallel visiting, because in general parts of
the tree may be visited multiple times, some parts may not be run at all on a
given processor, and whenever a leaf is hit there will be a jump to a higher
node, so if your tree builder is impure the effects need to be meaningful no
matter how the tree builder is run on a given processor.

Having said that, there are a few times when an impure tree builder can make
sense: first, if the inner monad is something like the `Reader` monad, which has
no side-effects;  second, for testing purposes (e.g., many of my tests of the
various tree builder visiors use `MVar`s and the like to ensure that tree
builders are explored in a certain way to test certain code paths);  finally, if
there is some side-effectful action that you want to run on each result (such as
storing a result into a database), though in this case you will need to make
sure that your code is robust against being run multiple times as there is no
guarantee in an environment where the system might be shut down and resumed from
a checkpoint that your action will only have been run once on a given result
(i.e., if the system goes down after your action was run but before a checkpoint
was made marking that its node was visited).

If you need something like state in your tree builder, then you should consider
nesting the tree builder monad in the state monad rather than vice-versa,
because this will do things like automatically erasing the change in state that
happened between an inner node and a leaf when the tree builder jumps back up
from the leaf to an inner node, which will usually be what you want.
-}

{-| A pure tree builder, which is what you should normally be using. -}
type TreeGenerator = TreeGeneratorT Identity

{-| A tree builder running in the I/O monad, which you should only be using for
    testing purposes or, say, if you are planning on storing each result in an
    external database, in which case you need to guard against the possibility
    that an action for a given result might be run twice in checkpointing and/or
    parallel settings.
-}
type TreeGeneratorIO = TreeGeneratorT IO

{-| A tree builder run in an arbitrary monad. -}
newtype TreeGeneratorT m α = TreeGeneratorT { unwrapTreeGeneratorT :: ProgramT (TreeGeneratorTInstruction m) m α }
    deriving (Applicative,Functor,Monad,MonadIO)

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{- $type-classes

'TreeGenerator's are instances of 'MonadVisitable' and/or 'MonadVisitableTrans',
which are both subclasses of 'MonadPlus'. The additional functionality offered
by these type-classes is the ability to cache results so that a computation does
not need to be repeated when a node is visited a second time, which can happen
either when resuming from a checkpoint or when a workload has been stolen by
another processor as the first step is to retrace the path through the tree
builder that leads to the stolen workload.

These features could have been provided as functions, but there are two reasons
why they were subsumed into type-classes: first, because one might want to
add another layer above the 'TreeGenerator' monad transformers in the monad stack
(as is the case in "Visitor.Label"), and second, because one might want
to run a tree builder using a simpler monad such as [] for testing purposes.

NOTE:  Caching a computation takes space in the 'Checkpoint', so it is something
       you should only do when the result is relatively small and the
       computation is very expensive and is high enough in the search tree that
       it is likely to be repeated often.  If the calculation is low enough in
       the search tree that it is unlikely to be repeated, is cheap enough so
       that repeating it is not a big deal, or produces a result with an
       incredibly large memory footprint, then you are probably better off not
       caching the result.
 -}

{-| The 'MonadVisitable' class provides caching functionality when visiting a
    tree;  at minimum 'cacheMaybe' needs to be defined.
 -}
class MonadPlus m ⇒ MonadVisitable m where
    {-| Cache a value in case we visit this node again. -}
    cache :: Serialize x ⇒ x → m x
    cache = cacheMaybe . Just

    {-| This does the same thing as 'guard' but it caches the result. -}
    cacheGuard :: Bool → m ()
    cacheGuard = cacheMaybe . (\x → if x then Just () else Nothing)

    {-| This function is a combination of the previous two;  it performs a
        computation which might fail by returning 'Nothing', and if that happens
        it aborts the tree builder;  if it passes then the result is cached and
        returned.

        Note that the previous two methods are essentially specializations of
        this method.
     -}
    cacheMaybe :: Serialize x ⇒ Maybe x → m x

{-| This class is like 'MonadVisitable', but it is designed to work with monad
    stacks;  at minimum 'runAndCacheMaybe' needs to be defined.
 -}
class (MonadPlus m, Monad (NestedMonadInVisitor m)) ⇒ MonadVisitableTrans m where
    {-| The next layer down in the monad transformer stack. -}
    type NestedMonadInVisitor m :: * → *

    {-| Runs the given action in the nested monad and caches the result. -}
    runAndCache :: Serialize x ⇒ (NestedMonadInVisitor m) x → m x
    runAndCache = runAndCacheMaybe . liftM Just

    {-| Runs the given action in the nested monad and then does the equivalent
        of feeding it into 'guard', caching the result.
     -}
    runAndCacheGuard :: (NestedMonadInVisitor m) Bool → m ()
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)

    {-| Runs the given action in the nested monad;  if it returns 'Nothing',
        then it acts like 'mzero',  if it returns 'Just x', then it caches the
        result.
     -}
    runAndCacheMaybe :: Serialize x ⇒ (NestedMonadInVisitor m) (Maybe x) → m x

--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------

{-| The 'Alternative' instance functions like the 'MonadPlus' instance. -}
instance Monad m ⇒ Alternative (TreeGeneratorT m) where
    empty = mzero
    (<|>) = mplus

{-| Two tree builders are equal if they build exactly the same tree. -}
instance Eq α ⇒ Eq (TreeGenerator α) where
    (TreeGeneratorT x) == (TreeGeneratorT y) = e x y
      where
        e x y = case (view x, view y) of
            (Return x, Return y) → x == y
            (Null :>>= _, Null :>>= _) → True
            (Cache cx :>>= kx, Cache cy :>>= ky) →
                case (runIdentity cx, runIdentity cy) of
                    (Nothing, Nothing) → True
                    (Just x, Just y) → e (kx x) (ky y)
                    _ → False
            (Choice (TreeGeneratorT ax) (TreeGeneratorT bx) :>>= kx, Choice (TreeGeneratorT ay) (TreeGeneratorT by) :>>= ky) →
                e (ax >>= kx) (ay >>= ky) && e (bx >>= kx) (by >>= ky)
            _  → False

{-| For this type, 'mplus' creates a branch node with a choice between two
    subtrees and 'mzero' aborts the tree builder.
 -}
instance Monad m ⇒ MonadPlus (TreeGeneratorT m) where
    mzero = TreeGeneratorT . singleton $ Null
    left `mplus` right = TreeGeneratorT . singleton $ Choice left right

{-| This instance performs no caching but is provided to make it easier to test
    running a tree builder using the List monad.
 -}
instance MonadVisitable [] where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a tree builder using the 'ListT' monad.
 -}
instance Monad m ⇒ MonadVisitable (ListT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadVisitable' isntance, this instance does no caching. -}
instance Monad m ⇒ MonadVisitableTrans (ListT m) where
    type NestedMonadInVisitor (ListT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a tree builder using the 'Maybe' monad.
 -}
instance MonadVisitable Maybe where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a tree builder using the 'MaybeT' monad.
 -}
instance Monad m ⇒ MonadVisitable (MaybeT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadVisitable' isntance, this instance does no caching. -}
instance Monad m ⇒ MonadVisitableTrans (MaybeT m) where
    type NestedMonadInVisitor (MaybeT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

instance Monad m ⇒ MonadVisitable (TreeGeneratorT m) where
    cache = runAndCache . return
    cacheGuard = runAndCacheGuard . return
    cacheMaybe = runAndCacheMaybe . return

instance Monad m ⇒ MonadVisitableTrans (TreeGeneratorT m) where
    type NestedMonadInVisitor (TreeGeneratorT m) = m
    runAndCache = runAndCacheMaybe . liftM Just
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)
    runAndCacheMaybe = TreeGeneratorT . singleton . Cache

{-| This instance allows you to automatically get a MonadVisitable instance for
    any monad transformer that has `MonadPlus` defined.  (Unfortunately its
    presence requires OverlappingInstances because it overlaps with the instance
    for `VisitorT`, even though the constraints are such that it is impossible
    in practice for there to ever be a case where a given type is satisfied by
    both instances.)
 -}
instance (MonadTrans t, MonadVisitable m, MonadPlus (t m)) ⇒ MonadVisitable (t m) where
    cache = lift . cache
    cacheGuard = lift . cacheGuard
    cacheMaybe = lift . cacheMaybe

instance MonadTrans TreeGeneratorT where
    lift = TreeGeneratorT . lift

{-| The 'Monoid' instance acts like the 'MonadPlus' instance. -}
instance Monad m ⇒ Monoid (TreeGeneratorT m α) where
    mempty = mzero
    mappend = mplus
    mconcat = msum

instance Show α ⇒ Show (TreeGenerator α) where
    show = s . unwrapTreeGeneratorT
      where
        s x = case view x of
            Return x → show x
            Null :>>= _ → "<NULL> >>= (...)"
            Cache c :>>= k →
                case runIdentity c of
                    Nothing → "NullCache"
                    Just x → "Cache[" ++ (show . encode $ x) ++ "] >>= " ++ (s (k x))
            Choice (TreeGeneratorT a) (TreeGeneratorT b) :>>= k → "(" ++ (s (a >>= k)) ++ ") | (" ++ (s (b >>= k)) ++ ")"


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{- $functions
There are three kinds of functions in this module: functions which visit trees
in various ways, functions to make it easier to build trees, and a function
which changes the base monad of a pure tree builder.
 -}

---------------------------------- Visitors ------------------------------------

{- $runners
The following functions all take a tree builder as input and produce the result
of visiting it as output. There are seven functions because there are two kinds
of tree builders -- pure and impure -- and three ways of visiting a tree --
visiting everything and summing all results (i.e., in the leaves), visiting
until the first result (i.e., in a leaf) is encountered and immediately
returning, and gathering results (i.e., from the leaves) until they satisfy a
condition and then return -- plus a seventh function that visits a tree only for
the side-effects in the tree builder.
 -}

{-| Visits all the nodes in a purely generated tree and sums over all the
    results in the leaves.
 -}
visitTree ::
    Monoid α ⇒
    TreeGenerator α {-^ the (pure) builder of the tree to be visited -} →
    α {-^ the sum over all results -}
visitTree v =
    case view (unwrapTreeGeneratorT v) of
        Return !x → x
        (Cache mx :>>= k) → maybe mempty (visitTree . TreeGeneratorT . k) (runIdentity mx)
        (Choice left right :>>= k) →
            let !x = visitTree $ left >>= TreeGeneratorT . k
                !y = visitTree $ right >>= TreeGeneratorT . k
                !xy = mappend x y
            in xy
        (Null :>>= _) → mempty
{-# INLINEABLE visitTree #-}

{-| Visits all the nodes in an impurely generated tree and sums over all the
    results in the leaves.
 -}
visitTreeT ::
    (Monad m, Monoid α) ⇒
    TreeGeneratorT m α {-^ the (impure) builder of the tree to be visited -} →
    m α {-^ the sum over all results -}
visitTreeT = viewT . unwrapTreeGeneratorT >=> \view →
    case view of
        Return !x → return x
        (Cache mx :>>= k) → mx >>= maybe (return mempty) (visitTreeT . TreeGeneratorT . k)
        (Choice left right :>>= k) →
            liftM2 (\(!x) (!y) → let !xy = mappend x y in xy)
                (visitTreeT $ left >>= TreeGeneratorT . k)
                (visitTreeT $ right >>= TreeGeneratorT . k)
        (Null :>>= _) → return mempty
{-# SPECIALIZE visitTreeT :: Monoid α ⇒ TreeGenerator α → Identity α #-}
{-# SPECIALIZE visitTreeT :: Monoid α ⇒ TreeGeneratorIO α → IO α #-}
{-# INLINEABLE visitTreeT #-}

{-| Visits a tree for the side-effects in its builder, ignoring all results. -}
visitTreeTAndIgnoreResults ::
    Monad m ⇒
    TreeGeneratorT m α {-^ the (impure) builder of the tree to be visited -} →
    m ()
visitTreeTAndIgnoreResults = viewT . unwrapTreeGeneratorT >=> \view →
    case view of
        Return _ → return ()
        (Cache mx :>>= k) → mx >>= maybe (return ()) (visitTreeTAndIgnoreResults . TreeGeneratorT . k)
        (Choice left right :>>= k) → do
            visitTreeTAndIgnoreResults $ left >>= TreeGeneratorT . k
            visitTreeTAndIgnoreResults $ right >>= TreeGeneratorT . k
        (Null :>>= _) → return ()
{-# SPECIALIZE visitTreeTAndIgnoreResults :: TreeGenerator α → Identity () #-}
{-# SPECIALIZE visitTreeTAndIgnoreResults :: TreeGeneratorIO α → IO () #-}
{-# INLINEABLE visitTreeTAndIgnoreResults #-}

{-| Visits all the nodes in a tree until a result (i.e., a leaf) has been found;
    if a result has been found then it is returned wrapped in 'Just', otherwise
    'Nothing' is returned.
 -}
visitTreeUntilFirst ::
    TreeGenerator α {-^ the (pure) builder of the tree to be visited -} →
    Maybe α {-^ the first result found, if any -}
visitTreeUntilFirst v =
    case view (unwrapTreeGeneratorT v) of
        Return x → Just x
        (Cache mx :>>= k) → maybe Nothing (visitTreeUntilFirst . TreeGeneratorT . k) (runIdentity mx)
        (Choice left right :>>= k) →
            let x = visitTreeUntilFirst $ left >>= TreeGeneratorT . k
                y = visitTreeUntilFirst $ right >>= TreeGeneratorT . k
            in if isJust x then x else y
        (Null :>>= _) → Nothing
{-# INLINEABLE visitTreeUntilFirst #-}

{-| Same as 'visitTreeUntilFirst', but taking an impure tree builder instead
    of pure one.
 -}
visitTreeTUntilFirst ::
    Monad m ⇒
    TreeGeneratorT m α {-^ the (impure) builder of the tree to be visited -} →
    m (Maybe α) {-^ the first result found, if any -}
visitTreeTUntilFirst = viewT . unwrapTreeGeneratorT >=> \view →
    case view of
        Return !x → return (Just x)
        (Cache mx :>>= k) → mx >>= maybe (return Nothing) (visitTreeTUntilFirst . TreeGeneratorT . k)
        (Choice left right :>>= k) → do
            x ← visitTreeTUntilFirst $ left >>= TreeGeneratorT . k
            if isJust x
                then return x
                else visitTreeTUntilFirst $ right >>= TreeGeneratorT . k
        (Null :>>= _) → return Nothing
{-# SPECIALIZE visitTreeTUntilFirst :: TreeGenerator α → Identity (Maybe α) #-}
{-# SPECIALIZE visitTreeTUntilFirst :: TreeGeneratorIO α → IO (Maybe α) #-}
{-# INLINEABLE visitTreeTUntilFirst #-}

{-| Visits all the nodes in a tree, summing all encountered results (i.e., in
    the leaves) until the current partial sum satisfies the condition provided
    by the first function; if this condition is ever satisfied then its result
    is returned in 'Right', otherwise the final sum is returned in 'Left'.
 -}
visitTreeUntilFound ::
    Monoid α ⇒
    (α → Maybe β) {-^ a function that determines when the desired results have
                      been found;  'Nothing' will cause the search to continue
                      whereas returning 'Just' will cause the search to stop and
                      the value in the 'Just' to be returned wrappedi n 'Right'
                   -} →
    TreeGenerator α {-^ the (pure) builder of the tree to be visited -} →
    Either α β {-^ if no acceptable results were found, then 'Left' with the sum
                   over all results;  otherwise 'Right' with the value returned
                   by the function in the first argument
                -}
visitTreeUntilFound f v =
    case view (unwrapTreeGeneratorT v) of
        Return x → runThroughFilter x
        (Cache mx :>>= k) →
            maybe (Left mempty) (visitTreeUntilFound f . TreeGeneratorT . k)
            $
            runIdentity mx
        (Choice left right :>>= k) →
            let x = visitTreeUntilFound f $ left >>= TreeGeneratorT . k
                y = visitTreeUntilFound f $ right >>= TreeGeneratorT . k
            in case (x,y) of
                (result@(Right _),_) → result
                (_,result@(Right _)) → result
                (Left a,Left b) → runThroughFilter (a <> b)
        (Null :>>= _) → Left mempty
  where
    runThroughFilter x = maybe (Left x) Right . f $ x

{-| Same as 'visitTreeUntilFound', but taking an impure tree builder instead of
    a pure tree builder.
 -}
visitTreeTUntilFound ::
    (Monad m, Monoid α) ⇒
    (α → Maybe β) {-^ a function that determines when the desired results have
                      been found;  'Nothing' will cause the search to continue
                      whereas returning 'Just' will cause the search to stop and
                      the value in the 'Just' to be returned wrappedi n 'Right'
                   -} →
    TreeGeneratorT m α {-^ the (impure) builder of the tree to be visited -} →
    m (Either α β) {-^ if no acceptable results were found, then 'Left' with the
                       sum over all results;  otherwise 'Right' with the value
                       returned by the function in the first argument
                    -}
visitTreeTUntilFound f = viewT . unwrapTreeGeneratorT >=> \view →
    case view of
        Return x → runThroughFilter x
        (Cache mx :>>= k) →
            mx
            >>=
            maybe (return (Left mempty)) (visitTreeTUntilFound f . TreeGeneratorT . k)
        (Choice left right :>>= k) → do
            x ← visitTreeTUntilFound f $ left >>= TreeGeneratorT . k
            case x of
                result@(Right _) → return result
                Left a → do
                    y ← visitTreeTUntilFound f $ right >>= TreeGeneratorT . k
                    case y of
                        result@(Right _) → return result
                        Left b → runThroughFilter (a <> b)
        (Null :>>= _) → return (Left mempty)
  where
    runThroughFilter x = return . maybe (Left x) Right . f $ x

---------------------------------- Builders ------------------------------------

{- $builders
The following functions all create a tree builder from various inputs. The
convention for suffixes is as follows: No suffix means that the tree will be
built in a naive fashion using 'msum', which takes each item in the list and
'mplus'es it with the resut of the list --- that is

>   msum [a,b,c,d]

which is equivalent to

>   a `mplus` (b `mplus` (c `mplus` (d `mplus` mzero)))

The downside of this approach is that it produces an incredibly unbalanced tree,
which will degrade parallization;  this is because if the tree is too
unbalanced then the work-stealing algorithm will either still only a small
piece of the remaining workload or nearly all of the remaining workload, and in
both cases a processor will end up with a small amount of work to do before it
finishes and immediately needs to steal another workload from someone else.

Given that a balanced tree is desirable, the Balanced functions work by copying
the input list into an array, starting with a range that covers the whole array,
and then splitting the range at every choice point until eventually the range
has length 1, in which case the element of the array is read;  the result is an
optimally balanced tree.

The downside of the Balanced functions is that they need to process the whole
list at once rather than one element at a time. The BalancedBottomUp functions
use a different algorithm that takes 33% less time than the Balanced algorithm
by processing each element one at a time and building the result tree using a
bottom-up approach rather than a top-down approach. For details of this
algoithm, see "Visitor.Utils.MonadPlusForest"; note that it is also possible
to get a slight speed-up by using the data structures in
"Visitor.Utils.MonadPlusForest" directly rather than implicitly through the
BalancedBottomUp functions.

The odd function out in this section is 'between', which takes the lower and
upper bound if an input range and returns a tree builder that generates an
optimally balanced search tree with all of the results in the range.
 -}

{-| Returns a tree builder that generates a tree with all of the results in the
    input list.

    WARNING: The generated tree will be such that every branch has one element
    in the left branch and the remaining elements in the right branch, which is
    heavily unbalanced and difficult to parallelize. You should consider using
    'allFromBalanced' and 'allFromBalancedBottomUp instead.
 -}
allFrom ::
    MonadPlus m ⇒
    [α] {-^ the list of results to generate in the resulting tree builder -} →
    m α {-^ a tree builder that builds a completely unbalanced tree with the given results -}
allFrom = msum . map return
{-# INLINE allFrom #-}

{-| Returns a tree builder that builds a tree with all of the results in the
    input list in an optimally balanced search tree.
 -}
allFromBalanced ::
    MonadPlus m ⇒
    [α] {-^ the list of results to generate in the resulting tree builder -} →
    m α {-^ a tree builder that builds an optimally balanced tree with the given results -} 
allFromBalanced [] = mzero
allFromBalanced x = go 0 end
  where
    end = length x - 1
    array = listArray (0,end) x

    go a b
      | a == b = return $ array ! a
      | otherwise = go a m `mplus` go (m+1) b
          where
            m = (a + b) `div` 2
{-# INLINE allFromBalanced #-}

{-| Returns a tree builder that generates all of the results in the input list in
    an approximately balanced tree with less overhead than 'allFromBalanced';
    see the documentation for this section and/or "Visitor.Utils.MonadPlusForest"
    for more information about the exact algorithm used.
 -}
allFromBalancedBottomUp ::
    MonadPlus m ⇒
    [α] {-^ the list of results to generate in the resulting tree builder -} →
    m α {-^ a tree builder that builds an approximately balanced tree with the given results -}
allFromBalancedBottomUp = go emptyForest
  where
    go !forest [] = consolidateForest forest
    go !forest (x:xs) = go (addToForest forest (return x)) xs
{-# INLINE allFromBalancedBottomUp #-}

{-| Returns a tree builder that builders an optimally balanced tree with all of
    the elements in the given (inclusive) range; if the lower bound is greater
    than the upper bound it returns 'mzero'.
 -}
between ::
    (Enum n, MonadPlus m) ⇒
    n {-^ the (inclusive) lower bound of the range -} →
    n {-^ the (inclusive) upper bound of the range -} →
    m n {-^ a tree builder that generates all the results in the range -}
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

{-| Returns a tree builder that merges all of the tree builders in the input
    list using an optimally balanced tree.
 -}
msumBalanced ::
    MonadPlus m ⇒
    [m α] {-^ the list of builders to merge -} →
    m α {-^ the merged tree builder -}
msumBalanced [] = mzero
msumBalanced x = go 0 end
  where
    end = length x - 1
    array = listArray (0,end) x

    go a b
      | a == b = array ! a
      | otherwise = go a m `mplus` go (m+1) b
          where
            m = (a + b) `div` 2
{-# INLINE msumBalanced #-}

{-| Returns a tree builder that merges all of the tree builders in the input
    list using an approximately balanced tree with less overhead than
    'msumBalanced'; see the documentation for this section and/or
    "Visitor.Utils.MonadPlusForest" for more information about the exact algorithm
    used.
 -}
msumBalancedBottomUp ::
    MonadPlus m ⇒
    [m α] {-^ the list of builders to merge -} →
    m α {-^ the merged tree builder -}
msumBalancedBottomUp = go emptyForest
  where
    go !forest [] = consolidateForest forest
    go !forest (x:xs) = go (addToForest forest x) xs
{-# INLINE msumBalancedBottomUp #-}

-------------------------------- Transformers ----------------------------------

{-| This function lets you take a pure tree builder and transform it into a tree
    builder with an arbitrary base monad.
 -}
endowTreeGenerator ::
    Monad m ⇒
    TreeGenerator α {-^ the pure tree builder to transformed into an impure tree builder -} →
    TreeGeneratorT m α {-^ the resulting impure tree builder -}
endowTreeGenerator tree_builder =
    case view . unwrapTreeGeneratorT $ tree_builder of
        Return x → return x
        Cache mx :>>= k →
            cacheMaybe (runIdentity mx) >>= endowTreeGenerator . TreeGeneratorT . k
        Choice left right :>>= k →
            mplus
                (endowTreeGenerator left >>= endowTreeGenerator . TreeGeneratorT . k)
                (endowTreeGenerator right >>= endowTreeGenerator . TreeGeneratorT . k)
        Null :>>= _ → mzero


--------------------------------------------------------------------------------
------------------------------- Implementation ---------------------------------
--------------------------------------------------------------------------------

{- $implementation
The implementation of the 'TreeGenerator' types uses the approach described in
"The Operational Monad Tutorial", published in Issue 15 of The Monad.Reader at
<http://themonadreader.wordpress.com/>;  specifically it uses the `operational`
package.  The idea is that a list of instructions are provided in
'TreeGeneratorTInstruction', and then the operational monad does all the heavy lifting
of turning them into a monad.
 -}

{-| The core of the implementation of 'TreeGenerator' is mostly contained in this
    type, which provides a list of primitive instructions for tree builders:
    'Cache', which caches a value, 'Choice', which signals a branch with two
    choices, and 'Null', which indicates that there are no more results.
 -}
data TreeGeneratorTInstruction m α where
    Cache :: Serialize α ⇒ m (Maybe α) → TreeGeneratorTInstruction m α
    Choice :: TreeGeneratorT m α → TreeGeneratorT m α → TreeGeneratorTInstruction m α
    Null :: TreeGeneratorTInstruction m α

{-| This is just a convenient alias for working with pure tree builder. -}
type TreeGeneratorInstruction = TreeGeneratorTInstruction Identity
