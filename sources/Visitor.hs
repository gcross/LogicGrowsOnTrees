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
    -- * TreeBuilder Features
    -- $type-classes
      MonadVisitable(..)
    , MonadVisitableTrans(..)
    -- * TreeBuilder Types
    -- $types
    , TreeBuilder
    , TreeBuilderIO
    , TreeBuilderT(..)
    -- * Functions
    -- $functions

    -- ** ...that run visitors
    -- $runners
    , visitTree
    , visitTreeT
    , visitTreeTAndIgnoreResults
    , visitTreeUntilFirst
    , visitTreeTUntilFirst
    , visitTreeUntilFound
    , visitTreeTUntilFound
    -- ** ...that help creating visitors
    -- $builders
    , allFrom
    , allFromBalanced
    , allFromBalancedGreedy
    , between
    , msumBalanced
    , msumBalancedGreedy
    -- ** ...that transform visitors
    , endowTreeBuilder
    -- * Implementation
    , TreeBuilderTInstruction(..)
    , TreeBuilderInstruction
    ) where

import Control.Applicative (Alternative(..),Applicative(..))
import Control.Monad (MonadPlus(..),(>=>),guard,liftM,liftM2,msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Operational (ProgramT,ProgramViewT(..),singleton,view,viewT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)

import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Maybe (isJust)
import Data.Monoid ((<>),Monoid(..))
import Data.Serialize (Serialize(),encode)

import Visitor.Utils.MonadStacks

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{- $type-classes

Visitors are instances of 'MonadVisitable' and/or 'MonadVisitableTrans', which are
both subclasses of 'MonadPlus'.  The additional functionality offered by these
type-classes is the ability to cache results so that a computation does not need
to be repeated when a path through the visitor is taken a second time, which
happens either when resuming from a checkpoint or when a workload has been
stolen by another processor as the first step is to retrace the path through the
visitor that leads to the stolen workload.

These features could have been provided as functions, but there are two reasons
why they were subsumed into type-classes: first, because one might want to
add another layer above the 'TreeBuilder' monad transformers in the monad stack
(as is the case in "Visitor.Label"), and second, because one might want
to run a visitor using a simpler monad such as [] for testing purposes.

NOTE:  Caching a computation takes space in the 'Checkpoint', so it is something
       you should only do when the result is relatively small and the
       computation is very expensive and is high enough in the search tree that
       it is likely to be repeated often.  If the calculation is low enough in
       the search tree that it is unlikely to be repeated, is cheap enough so
       that repeating it is not a big deal, or produces a result with an
       incredibly large memory footprint, then you are probably better off not
       caching the result.
 -}


{-| The 'MonadVisitable' class provides caching functionality when running a pure
    visitor;  at minimum 'cacheMaybe' needs to be defined.
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
        it aborts the visitor;  if it passes then the result is cached and
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
------------------------------------- Types ------------------------------------
--------------------------------------------------------------------------------

{- $types
The following are the visitor types that are accepted by most of he functions in
this package.  You do not need to know the details of their definitions unless
you intend to write your own custom routines for running and transforming
visitors, in which case the relevant information is at the bottom of this page
in the Implementation` section.

There is one type of pure visitor and two types of impure visitors.  In general,
your visitor should nearly always be pure if you are planning to make use of
checkpointing or parallel runs, because in general parts of the visitor will be
run multiple times, some parts may not be run at all on a given processor, and
whenever a leaf is hit there will be a jump to a higher node, so if your visitor
is impure the effects need to be meaningful no matter how the visitor is run on
a given processor.

Having said that, there are a few times when an impure visitor can make sense:
first, if the inner monad is something like the `Reader` monad, which has no
side-effects;  second, for testing purposes (e.g., many of my tests of the
various visitor runners use `MVar`s and the like to ensure that visitors are
explored in a certain way to test certain code paths);  finally, if there is some
side-effectful action that you want to run on each result (such as storing a
result into a database), though in this case you will need to make sure that
your code is robust against being run multiple times as there is no guarantee in
an environment where the system might be shut down and resumed from a checkpoint
that your action will only have been run once on a given result (i.e., if the
system goes down after your action was run but before a checkpoint was made
marking that it was run).

If you need something like state in your visitor, then you should consider
nesting the visitor monad in the state monad rather than vice-versa, because
this will do things like automatically erasing the change in state that happened
between an inner node and a leaf when the visitor jumps back up from the leaf
to an inner node.
-}

{-| A pure visitor, which is what you should normally be using. -}
type TreeBuilder = TreeBuilderT Identity

{-| A visitor running in the I/O monad, which you should only be using for
    testing purposes or, say, if you are planning on storing each result in an
    external database, in which case you need to guard against the possibility
    that an action for a given result might be run twice in checkpointing and/or
    parallel settings.
-}
type TreeBuilderIO = TreeBuilderT IO

{-| A visitor run in an arbitrary monad. -}
newtype TreeBuilderT m α = TreeBuilderT { unwrapTreeBuilderT :: ProgramT (TreeBuilderTInstruction m) m α }
    deriving (Applicative,Functor,Monad,MonadIO)

--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------

{-| The 'Alternative' instance functions like the 'MonadPlus' instance. -}
instance Monad m ⇒ Alternative (TreeBuilderT m) where
    empty = mzero
    (<|>) = mplus

{-| Two visitors are equal if the generate exactly the same tree. -}
instance Eq α ⇒ Eq (TreeBuilder α) where
    (TreeBuilderT x) == (TreeBuilderT y) = e x y
      where
        e x y = case (view x, view y) of
            (Return x, Return y) → x == y
            (Null :>>= _, Null :>>= _) → True
            (Cache cx :>>= kx, Cache cy :>>= ky) →
                case (runIdentity cx, runIdentity cy) of
                    (Nothing, Nothing) → True
                    (Just x, Just y) → e (kx x) (ky y)
                    _ → False
            (Choice (TreeBuilderT ax) (TreeBuilderT bx) :>>= kx, Choice (TreeBuilderT ay) (TreeBuilderT by) :>>= ky) →
                e (ax >>= kx) (ay >>= ky) && e (bx >>= kx) (by >>= ky)
            _  → False

{-| For this type, 'mplus' provides a branch node with a choice between two
    values and 'mzero' provides a node with no values.
 -}
instance Monad m ⇒ MonadPlus (TreeBuilderT m) where
    mzero = TreeBuilderT . singleton $ Null
    left `mplus` right = TreeBuilderT . singleton $ Choice left right

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the List monad.
 -}
instance MonadVisitable [] where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the 'ListT' monad.
 -}
instance Monad m ⇒ MonadVisitable (ListT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadVisitable' isntance, this instance does no caching. -}
instance Monad m ⇒ MonadVisitableTrans (ListT m) where
    type NestedMonadInVisitor (ListT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the 'Maybe' monad.
 -}
instance MonadVisitable Maybe where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the 'MaybeT' monad.
 -}
instance Monad m ⇒ MonadVisitable (MaybeT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadVisitable' isntance, this instance does no caching. -}
instance Monad m ⇒ MonadVisitableTrans (MaybeT m) where
    type NestedMonadInVisitor (MaybeT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

instance Monad m ⇒ MonadVisitable (TreeBuilderT m) where
    cache = runAndCache . return
    cacheGuard = runAndCacheGuard . return
    cacheMaybe = runAndCacheMaybe . return

instance Monad m ⇒ MonadVisitableTrans (TreeBuilderT m) where
    type NestedMonadInVisitor (TreeBuilderT m) = m
    runAndCache = runAndCacheMaybe . liftM Just
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)
    runAndCacheMaybe = TreeBuilderT . singleton . Cache

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

instance MonadTrans TreeBuilderT where
    lift = TreeBuilderT . lift

{-| The 'Monoid' instance acts like the 'MonadPlus' instance. -}
instance Monad m ⇒ Monoid (TreeBuilderT m α) where
    mempty = mzero
    mappend = mplus
    mconcat = msum

instance Show α ⇒ Show (TreeBuilder α) where
    show = s . unwrapTreeBuilderT
      where
        s x = case view x of
            Return x → show x
            Null :>>= _ → "<NULL> >>= (...)"
            Cache c :>>= k →
                case runIdentity c of
                    Nothing → "NullCache"
                    Just x → "Cache[" ++ (show . encode $ x) ++ "] >>= " ++ (s (k x))
            Choice (TreeBuilderT a) (TreeBuilderT b) :>>= k → "(" ++ (s (a >>= k)) ++ ") | (" ++ (s (b >>= k)) ++ ")"


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{- $functions
There are three kinds of functions in this module:  functions which run visitors
in various ways, functions to make it easier to build visitors, and functions
which change the base monad of a pure visitor.
 -}

----------------------------------- Runners ------------------------------------

{- $runners
The following functions all take a visitor as input and produce the resul of
running the visitor as output.  There are seven functions because there are two
kinds of visitors -- pure and impure -- and three modes for running the visitor
-- sum all result, return the first result, and gather results until they
satisfy a condition and then return -- plus a seventh function that runs a
visitor only for its side-effects.
 -}

{-| Run a pure visitor until all results have been found and summed together. -}
visitTree ::
    Monoid α ⇒
    TreeBuilder α {-^ the (pure) visitor to run -} →
    α {-^ the sum over all results -}
visitTree v =
    case view (unwrapTreeBuilderT v) of
        Return !x → x
        (Cache mx :>>= k) → maybe mempty (visitTree . TreeBuilderT . k) (runIdentity mx)
        (Choice left right :>>= k) →
            let !x = visitTree $ left >>= TreeBuilderT . k
                !y = visitTree $ right >>= TreeBuilderT . k
                !xy = mappend x y
            in xy
        (Null :>>= _) → mempty
{-# INLINEABLE visitTree #-}

{-| Run an impure visitor until all results have been found and summed together. -}
visitTreeT ::
    (Monad m, Monoid α) ⇒
    TreeBuilderT m α {-^ the (impure) visitor to run -} →
    m α {-^ the sum over all results -}
visitTreeT = viewT . unwrapTreeBuilderT >=> \view →
    case view of
        Return !x → return x
        (Cache mx :>>= k) → mx >>= maybe (return mempty) (visitTreeT . TreeBuilderT . k)
        (Choice left right :>>= k) →
            liftM2 (\(!x) (!y) → let !xy = mappend x y in xy)
                (visitTreeT $ left >>= TreeBuilderT . k)
                (visitTreeT $ right >>= TreeBuilderT . k)
        (Null :>>= _) → return mempty
{-# SPECIALIZE visitTreeT :: Monoid α ⇒ TreeBuilder α → Identity α #-}
{-# SPECIALIZE visitTreeT :: Monoid α ⇒ TreeBuilderIO α → IO α #-}
{-# INLINEABLE visitTreeT #-}

{-| Run an impure visitor for its side-effects, ignoring all results. -}
visitTreeTAndIgnoreResults ::
    Monad m ⇒
    TreeBuilderT m α {-^ the (impure) visitor to run -} →
    m ()
visitTreeTAndIgnoreResults = viewT . unwrapTreeBuilderT >=> \view →
    case view of
        Return _ → return ()
        (Cache mx :>>= k) → mx >>= maybe (return ()) (visitTreeTAndIgnoreResults . TreeBuilderT . k)
        (Choice left right :>>= k) → do
            visitTreeTAndIgnoreResults $ left >>= TreeBuilderT . k
            visitTreeTAndIgnoreResults $ right >>= TreeBuilderT . k
        (Null :>>= _) → return ()
{-# SPECIALIZE visitTreeTAndIgnoreResults :: TreeBuilder α → Identity () #-}
{-# SPECIALIZE visitTreeTAndIgnoreResults :: TreeBuilderIO α → IO () #-}
{-# INLINEABLE visitTreeTAndIgnoreResults #-}

{-| Run a pure visitor until a result has been found;  if a result has been
    found then it is returned wrapped in 'Just', otherwise 'Nothing' is returned.
 -}
visitTreeUntilFirst ::
    TreeBuilder α {-^ the (pure) visitor to run -} →
    Maybe α {-^ the first result found, if any -}
visitTreeUntilFirst v =
    case view (unwrapTreeBuilderT v) of
        Return x → Just x
        (Cache mx :>>= k) → maybe Nothing (visitTreeUntilFirst . TreeBuilderT . k) (runIdentity mx)
        (Choice left right :>>= k) →
            let x = visitTreeUntilFirst $ left >>= TreeBuilderT . k
                y = visitTreeUntilFirst $ right >>= TreeBuilderT . k
            in if isJust x then x else y
        (Null :>>= _) → Nothing
{-# INLINEABLE visitTreeUntilFirst #-}

{-| Same as 'visitTreeUntilFirst', but taking an impure visitor instead of a pure visitor. -}
visitTreeTUntilFirst ::
    Monad m ⇒
    TreeBuilderT m α {-^ the (impure) visitor to run -} →
    m (Maybe α) {-^ the first result found, if any -}
visitTreeTUntilFirst = viewT . unwrapTreeBuilderT >=> \view →
    case view of
        Return !x → return (Just x)
        (Cache mx :>>= k) → mx >>= maybe (return Nothing) (visitTreeTUntilFirst . TreeBuilderT . k)
        (Choice left right :>>= k) → do
            x ← visitTreeTUntilFirst $ left >>= TreeBuilderT . k
            if isJust x
                then return x
                else visitTreeTUntilFirst $ right >>= TreeBuilderT . k
        (Null :>>= _) → return Nothing
{-# SPECIALIZE visitTreeTUntilFirst :: TreeBuilder α → Identity (Maybe α) #-}
{-# SPECIALIZE visitTreeTUntilFirst :: TreeBuilderIO α → IO (Maybe α) #-}
{-# INLINEABLE visitTreeTUntilFirst #-}

{-| Run a pure visitor summing all results encountered until the current sum
    satisfies the condition provided by the function in the first argument;  if
    the sum never satisfies the condition function then it (that is, the sum
    over all results) is returned wrapped in 'Left', otherwise the transformed
    result returned by the condition function is returned wrapped in 'Right'.
 -}
visitTreeUntilFound ::
    Monoid α ⇒
    (α → Maybe β) {-^ a function that determines when the desired results have
                      been found;  'Nothing' will cause the search to continue
                      whereas returning 'Just' will cause the search to stop and
                      the value in the 'Just' to be returned wrappedi n 'Right'
                   -} →
    TreeBuilder α  {-^ the (pure) visitor to run -} →
    Either α β {-^ if no acceptable results were found, then 'Left' with the sum
                   over all results;  otherwise 'Right' with the value returned
                   by the function in the first argument
                -}
visitTreeUntilFound f v =
    case view (unwrapTreeBuilderT v) of
        Return x → runThroughFilter x
        (Cache mx :>>= k) →
            maybe (Left mempty) (visitTreeUntilFound f . TreeBuilderT . k)
            $
            runIdentity mx
        (Choice left right :>>= k) →
            let x = visitTreeUntilFound f $ left >>= TreeBuilderT . k
                y = visitTreeUntilFound f $ right >>= TreeBuilderT . k
            in case (x,y) of
                (result@(Right _),_) → result
                (_,result@(Right _)) → result
                (Left a,Left b) → runThroughFilter (a <> b)
        (Null :>>= _) → Left mempty
  where
    runThroughFilter x = maybe (Left x) Right . f $ x

{-| Same as 'visitTreeUntilFound', but taking an impure visitor instead of a pure visitor. -}
visitTreeTUntilFound ::
    (Monad m, Monoid α) ⇒
    (α → Maybe β) {-^ a function that determines when the desired results have
                      been found;  'Nothing' will cause the search to continue
                      whereas returning 'Just' will cause the search to stop and
                      the value in the 'Just' to be returned wrappedi n 'Right'
                   -} →
    TreeBuilderT m α {-^ the (impure) visitor to run -} →
    m (Either α β) {-^ if no acceptable results were found, then 'Left' with the
                       sum over all results;  otherwise 'Right' with the value
                       returned by the function in the first argument
                    -}
visitTreeTUntilFound f = viewT . unwrapTreeBuilderT >=> \view →
    case view of
        Return x → runThroughFilter x
        (Cache mx :>>= k) →
            mx
            >>=
            maybe (return (Left mempty)) (visitTreeTUntilFound f . TreeBuilderT . k)
        (Choice left right :>>= k) → do
            x ← visitTreeTUntilFound f $ left >>= TreeBuilderT . k
            case x of
                result@(Right _) → return result
                Left a → do
                    y ← visitTreeTUntilFound f $ right >>= TreeBuilderT . k
                    case y of
                        result@(Right _) → return result
                        Left b → runThroughFilter (a <> b)
        (Null :>>= _) → return (Left mempty)
  where
    runThroughFilter x = return . maybe (Left x) Right . f $ x

---------------------------------- Builders ------------------------------------

{- $builders
The following functions all construct a visitor from various inputs.  The
convention for suffixes is as follows:  No suffix means that the visitor will be
constructed in a naive fashion using 'msum', which takes each item in the list
and 'mplus'es it with the resut of the list --- that is

>   msum [a,b,c,d]

is equivalent to

>   a `mplus` (b `mplus` (c `mplus` (d `mplus` mzero)))

The downside of this approach is that it produces an incredibly unbalanced tree,
which will degrade parallization;  this is because if the tree is too
unbalanced then the work-stealing algorithm will either still only a small
piece of the remaining workload or nearly all of the remaining workload, and in
both cases a processor will end up with a small amount of work to do before it
finishes and immediately needs to steal another workload from someone else.

Given that a balanced tree is desirable, the Balanced functions work by
splitting the input list and then recursively processing each half;  this
creates a search tree that is as balanced as it can be.

The downside of the Balanced functions is that they need to scan the list in
each call in order to split it in half, which adds some performance overhead.
Thus, the BalancedGreedy functions' approach is to build up a tree in a
`greedy` manner by doing the following at each step:  take the current element
and put it in a new `stack`; while there exists a stack of the same size as the
current stack, merge that stack with the current stack.  When all of the
elements have been processed, add up all the stacks starting with the smallest
and ending with the largest. As each of the stacks is a perfectly balanced
subtree, and the largest stack has a size at least as great as total number of
elements in the other stacks, the end result is a search tree where at least
half of the elements are in a perfectly balanced subtree;  this is often close
enough to being fully balanced tree.

The odd function out in this section is 'between', which takes the lower and
upper bound if an input range and returns a visitor that generates an optimally
balanced search tree with all of the results in the range.
 -}

{-| Returns a visitor that generates all of the results in the input list.

    WARNING:  The generated search tree will be such that every branch has one
    element in the left branch and the remaining elements in the right branch,
    which is heavily unbalanced and difficult to parallelize.  You should
    consider using 'allFromBalanced' and 'allFromBalancedGreedy instead.
 -}
allFrom ::
    MonadPlus m ⇒
    [α] {-^ the list of results to generate in the visitor -} →
    m α {-^ a completely unbalanced visitor generating the input results -}
allFrom = msum . map return
{-# INLINE allFrom #-}

{-| Returns a visitor that generates all of the results in the input list in
    an optimally balanced search tree.
 -}
allFromBalanced ::
    MonadPlus m ⇒
    [α] {-^ the list of results to generate in the visitor -} →
    m α {-^ the optimally balanced visitor generating the input results -} 
allFromBalanced = msumBalanced . map return
{-# INLINE allFromBalanced #-}

{-| Returns a visitor that generates all of the results in the input list in
    an approximately balanced search tree with less overhead than
    'allFromBalanced';  see the documentation for this section and/or
    "Visitor.Utils.MonadStacks" for more information about the exact
    algorithm used.
 -}
allFromBalancedGreedy ::
    MonadPlus m ⇒
    [α] {-^ the list of results to generate in the visitor -} →
    m α {-^ an approximately balanced visitor generating the input results -}
allFromBalancedGreedy = msumBalancedGreedy . map return
{-# INLINE allFromBalancedGreedy #-}

{-| Returns a visitor that generates an optimally balanced search tree with all
    of the elements in the given (inclusive) range;  if the lower bound is
    greater than the upper bound it returns 'mzero'.
 -}
between ::
    (Enum n, MonadPlus m) ⇒
    n {-^ the (inclusive) lower bound of the range -} →
    n {-^ the (inclusive) upper bound of the range -} →
    m n {-^ a visitor that generates all the results in the range -}
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

{-| Returns a visitor that merges all of the visitors in the input list using
    an optimally balanced search tree.
 -}
msumBalanced ::
    MonadPlus m ⇒
    [m α] {-^ the list of visitors to merge -} →
    m α {-^ the merged visitor -}
msumBalanced x = go (length x) x
  where
    go _ []  = mzero
    go _ [x] = x
    go n x   = go i a `mplus` go (n-i) b
      where
        (a,b) = splitAt i x
        i = n `div` 2
{-# INLINE msumBalanced #-}

{-| Returns a visitor that merges all of the visitors in the input list using
    an approximately balanced search tree with less overhead than
    'msumBalanced';  see the documentation for this section and/or
    "Visitor.Utils.MonadStacks" for more information about the exact
    algorithm used.
 -}
msumBalancedGreedy ::
    MonadPlus m ⇒
    [m α] {-^ the list of visitors to merge -} →
    m α {-^ the merged visitor -}
msumBalancedGreedy = go emptyStacks
  where
    go !stacks [] = mergeStacks stacks
    go !stacks (x:xs) = go (addToStacks stacks x) xs
{-# INLINE msumBalancedGreedy #-}

-------------------------------- Transformers ----------------------------------

{-| This function lets you take a pure visitor and transform it into a visitor
    with an arbitrary base monad.
 -}
endowTreeBuilder ::
    Monad m ⇒
    TreeBuilder α {-^ the pure visitor to transformed into an impure visitor -} →
    TreeBuilderT m α {-^ the resulting impure visitor -}
endowTreeBuilder visitor =
    case view . unwrapTreeBuilderT $ visitor of
        Return x → return x
        Cache mx :>>= k →
            cacheMaybe (runIdentity mx) >>= endowTreeBuilder . TreeBuilderT . k
        Choice left right :>>= k →
            mplus
                (endowTreeBuilder left >>= endowTreeBuilder . TreeBuilderT . k)
                (endowTreeBuilder right >>= endowTreeBuilder . TreeBuilderT . k)
        Null :>>= _ → mzero


--------------------------------------------------------------------------------
------------------------------- Implementation ---------------------------------
--------------------------------------------------------------------------------

{- $implementation
The implementation of the 'TreeBuilder' types uses the approach described in
"The Operational Monad Tutorial", published in Issue 15 of The Monad.Reader at
<http://themonadreader.wordpress.com/>;  specifically it uses the `operational`
package.  The idea is that a list of instructions are provided in
'TreeBuilderTInstruction', and then the operational monad does all the heavy lifting
of turning them into a monad.
 -}

{-| The core of the implementation of 'TreeBuilder' is mostly contained in this
    type, which provides a list of primitive instructions for visitors: 'Cache',
    which caches a value, 'Choice', which signals a branch with two choices, and
    'Null', which indicates that there are no more results.
 -}
data TreeBuilderTInstruction m α where
    Cache :: Serialize α ⇒ m (Maybe α) → TreeBuilderTInstruction m α
    Choice :: TreeBuilderT m α → TreeBuilderT m α → TreeBuilderTInstruction m α
    Null :: TreeBuilderTInstruction m α

{-| This is just a convenient alias for working with pure visitors. -}
type TreeBuilderInstruction = TreeBuilderTInstruction Identity
