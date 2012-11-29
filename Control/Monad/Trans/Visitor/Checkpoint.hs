-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Checkpoint where

-- Imports {{{
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join)
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)
import Data.Composition
import Data.Functor.Identity (Identity,runIdentity)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(),decode,encode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Path
-- }}}

-- Types {{{

data VisitorCheckpoint = -- {{{
    CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Explored
  | Unexplored
  deriving (Eq,Read,Show)
-- }}}

type VisitorCheckpointCursor = Seq VisitorCheckpointDifferential

data VisitorCheckpointDifferential = -- {{{
    CacheCheckpointD ByteString
  | ChoiceCheckpointD Branch VisitorCheckpoint
  deriving (Eq,Read,Show)
-- }}}

type VisitorTContext m α = Seq (VisitorTContextStep m α)
type VisitorContext α = VisitorTContext Identity α

data VisitorTContextStep m α = -- {{{
    CacheContextStep ByteString
  | LeftBranchContextStep VisitorCheckpoint (VisitorT m α)
  | RightBranchContextStep
-- }}}
type VisitorContextStep = VisitorTContextStep Identity

type VisitorTContextUpdate m α = -- {{{
    VisitorTContext m α →
    Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α)
-- }}}

type VisitorContextUpdate α = VisitorTContextUpdate Identity α

data VisitorProgress α = VisitorProgress -- {{{
    {   visitorCheckpoint :: VisitorCheckpoint
    ,   visitorResult :: α
    } deriving (Eq,Show)
-- }}}

newtype VisitorTResultFetcher m α = VisitorTResultFetcher -- {{{
    {   fetchVisitorTResult :: m (Maybe (α, VisitorCheckpoint, VisitorTResultFetcher m α))
    }
-- }}}
type VisitorResultFetcher = VisitorTResultFetcher Identity

-- }}}

-- Exceptions {{{

data InconsistentCheckpoints = InconsistentCheckpoints VisitorCheckpoint VisitorCheckpoint deriving (Eq,Show,Typeable)

instance Exception InconsistentCheckpoints

-- }}}

-- Instances {{{

instance Monoid VisitorCheckpoint where -- {{{
    mempty = Unexplored
    Explored `mappend` _ = Explored
    _ `mappend` Explored = Explored
    Unexplored `mappend` x = x
    x `mappend` Unexplored = x
    (ChoiceCheckpoint lx rx) `mappend` (ChoiceCheckpoint ly ry) = mergeCheckpointRoot (ChoiceCheckpoint (lx ⊕ ly) (rx ⊕ ry))
    (CacheCheckpoint cx x) `mappend` (CacheCheckpoint cy y)
      | cx == cy = mergeCheckpointRoot (CacheCheckpoint cx (x ⊕ y))
    mappend x y = throw (InconsistentCheckpoints x y)
-- }}}

instance Monoid α ⇒ Monoid (VisitorProgress α) where -- {{{
    mempty = VisitorProgress mempty mempty
    VisitorProgress a1 b1 `mappend` VisitorProgress a2 b2 = VisitorProgress (a1 `mappend` a2) (b1 `mappend` b2)
-- }}}

instance Show (VisitorTContextStep m α) where -- {{{
    show (CacheContextStep c) = "CacheContextStep[" ++ show c ++ "]"
    show (LeftBranchContextStep checkpoint _) = "LeftBranchContextStep(" ++ show checkpoint ++ ")"
    show RightBranchContextStep = "RightRightBranchContextStep"
-- }}}

-- }}}

-- Functions {{{

checkpointFromContext :: VisitorTContext m α → VisitorCheckpoint → VisitorCheckpoint -- {{{
checkpointFromContext = checkpointFromSequence $
    \step → case step of
        CacheContextStep cache → CacheCheckpoint cache
        LeftBranchContextStep right_checkpoint _ → flip ChoiceCheckpoint right_checkpoint
        RightBranchContextStep → ChoiceCheckpoint Explored
-- }}}

checkpointFromCursor :: VisitorCheckpointCursor → VisitorCheckpoint → VisitorCheckpoint -- {{{
checkpointFromCursor = checkpointFromSequence $
    \step → case step of
        CacheCheckpointD cache → CacheCheckpoint cache
        ChoiceCheckpointD LeftBranch right_checkpoint → flip ChoiceCheckpoint right_checkpoint
        ChoiceCheckpointD RightBranch left_checkpoint → ChoiceCheckpoint left_checkpoint
-- }}}

checkpointFromInitialPath :: VisitorPath → VisitorCheckpoint → VisitorCheckpoint -- {{{
checkpointFromInitialPath = checkpointFromSequence $
    \step → case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Unexplored
        ChoiceStep RightBranch → ChoiceCheckpoint Unexplored
-- }}}

checkpointFromSequence :: -- {{{
    (α → (VisitorCheckpoint → VisitorCheckpoint)) →
    Seq α →
    VisitorCheckpoint →
    VisitorCheckpoint
checkpointFromSequence processStep (viewr → EmptyR) = id
checkpointFromSequence processStep (viewr → rest :> step) =
    checkpointFromSequence processStep rest
    .
    mergeCheckpointRoot
    .
    processStep step
-- }}}

checkpointFromUnexploredPath :: VisitorPath → VisitorCheckpoint -- {{{
checkpointFromUnexploredPath (viewl → EmptyL) = Unexplored
checkpointFromUnexploredPath (viewl → step :< rest_path) =
    case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Explored
        ChoiceStep RightBranch → ChoiceCheckpoint Explored
    $
    checkpointFromUnexploredPath rest_path
-- }}}

gatherResults :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorTResultFetcher m α →
    m α
gatherResults = go mempty
  where
    go result =
        fetchVisitorTResult
        >=>
        maybe
            (return result)
            (\(result,_,fetcher) → go result fetcher)
-- }}}

mergeAllCheckpointNodes :: VisitorCheckpoint → VisitorCheckpoint -- {{{
mergeAllCheckpointNodes (ChoiceCheckpoint left right) = mergeCheckpointRoot (ChoiceCheckpoint (mergeAllCheckpointNodes left) (mergeAllCheckpointNodes right))
mergeAllCheckpointNodes (CacheCheckpoint cache checkpoint) = mergeCheckpointRoot (CacheCheckpoint cache (mergeAllCheckpointNodes checkpoint))
mergeAllCheckpointNodes checkpoint = checkpoint
-- }}}

mergeCheckpointRoot :: VisitorCheckpoint → VisitorCheckpoint -- {{{
mergeCheckpointRoot (ChoiceCheckpoint Unexplored Unexplored) = Unexplored
mergeCheckpointRoot (ChoiceCheckpoint Explored Explored) = Explored
mergeCheckpointRoot (CacheCheckpoint _ Explored) = Explored
mergeCheckpointRoot checkpoint = checkpoint
-- }}}

moveDownContext :: -- {{{
    VisitorTContextStep m α →
    VisitorCheckpoint →
    VisitorT m α →
    VisitorTContextUpdate m α
moveDownContext step checkpoint visitor =
    Just
    .
    (,checkpoint,visitor)
    .
    (|> step)
-- }}}

moveUpContext :: VisitorTContextUpdate m α -- {{{
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> CacheContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> LeftBranchContextStep right_checkpoint right_visitor) =
    Just (rest_context |> RightBranchContextStep
         ,right_checkpoint
         ,right_visitor
         )
moveUpContext (viewr → rest_context :> RightBranchContextStep) = moveUpContext rest_context
-- }}}

pathFromContext :: VisitorTContext m α → VisitorPath -- {{{
pathFromContext = fmap pathStepFromContextStep
-- }}}

pathFromCursor :: VisitorCheckpointCursor → VisitorPath -- {{{
pathFromCursor = fmap pathStepFromCursorDifferential
-- }}}

pathStepFromContextStep :: VisitorTContextStep m α → VisitorStep -- {{{
pathStepFromContextStep (CacheContextStep cache) = CacheStep cache
pathStepFromContextStep (LeftBranchContextStep _ _) = ChoiceStep LeftBranch
pathStepFromContextStep (RightBranchContextStep) = ChoiceStep RightBranch
-- }}}

pathStepFromCursorDifferential :: VisitorCheckpointDifferential → VisitorStep -- {{{
pathStepFromCursorDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromCursorDifferential (ChoiceCheckpointD active_branch _) = ChoiceStep active_branch
-- }}}

runVisitorThroughCheckpoint :: -- {{{
    Monoid α ⇒
    VisitorCheckpoint →
    Visitor α →
    α
runVisitorThroughCheckpoint = (fst . last) .* walkVisitorThroughCheckpoint
-- }}}

runVisitorTThroughCheckpoint :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    m α
runVisitorTThroughCheckpoint = gatherResults .* walkVisitorTThroughCheckpoint
-- }}}

stepVisitorThroughCheckpoint :: -- {{{
    VisitorContext α →
    VisitorCheckpoint →
    Visitor α →
    (Maybe α,Maybe (VisitorContext α, VisitorCheckpoint, Visitor α))
stepVisitorThroughCheckpoint context checkpoint = runIdentity . stepVisitorTThroughCheckpoint context checkpoint
-- }}}

stepVisitorTThroughCheckpoint :: -- {{{
    (Functor m, Monad m) ⇒
    VisitorTContext m α →
    VisitorCheckpoint →
    VisitorT m α →
    m (Maybe α,Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α))
stepVisitorTThroughCheckpoint context Explored = const $ return (Nothing,moveUpContext context)
stepVisitorTThroughCheckpoint context checkpoint = viewT . unwrapVisitorT >=> \view → case (view,checkpoint) of
    (Return x,Unexplored) → return (Just x, moveUpMyContext)
    (Null :>>= _,Unexplored) → return (Nothing, moveUpMyContext)
    (Cache mx :>>= k,CacheCheckpoint cache rest_checkpoint) →
        return
        .
        (Nothing,)
        .
        moveDownMyContext (CacheContextStep cache) rest_checkpoint
        .
        either error (VisitorT . k)
        .
        decode
        $
        cache
    (Cache mx :>>= k,Unexplored) →
        mx >>= return . maybe
            (Nothing, moveUpMyContext)
            (\x → (Nothing,) $
                moveDownMyContext
                    (CacheContextStep (encode x))
                    Unexplored
                    (VisitorT . k $ x)
            )
    (Choice left right :>>= k, ChoiceCheckpoint left_checkpoint right_checkpoint) → return
        (Nothing
        ,moveDownMyContext
            (LeftBranchContextStep right_checkpoint (right >>= VisitorT . k))
            left_checkpoint
            (left >>= VisitorT . k)
        )
    (Choice left right :>>= k,Unexplored) → return
        (Nothing
        ,moveDownMyContext
            (LeftBranchContextStep Unexplored (right >>= VisitorT . k))
            Unexplored
            (left >>= VisitorT . k)
        )
    _ → throw PastVisitorIsInconsistentWithPresentVisitor
  where
    moveUpMyContext = moveUpContext context
    moveDownMyContext step checkpoint visitor = moveDownContext step checkpoint visitor context
-- }}}

walkVisitor :: -- {{{
    Monoid α ⇒
    Visitor α →
    [(α,VisitorCheckpoint)]
walkVisitor = walkVisitorThroughCheckpoint Unexplored
-- }}}

walkVisitorT :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorT m α →
    VisitorTResultFetcher m α
walkVisitorT = walkVisitorTThroughCheckpoint Unexplored
-- }}}

walkVisitorThroughCheckpoint :: -- {{{
    Monoid α ⇒
    VisitorCheckpoint →
    Visitor α →
    [(α,VisitorCheckpoint)]
walkVisitorThroughCheckpoint = go True .* walkVisitorTThroughCheckpoint
  where
    go True (runIdentity . fetchVisitorTResult → Nothing) = [(mempty,Explored)]
    go False (runIdentity . fetchVisitorTResult → Nothing) = []
    go _ (runIdentity . fetchVisitorTResult → Just (next_accum,checkpoint,next_result)) =
      (next_accum,checkpoint)
      :
      go False next_result
-- }}}

walkVisitorTThroughCheckpoint :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    VisitorTResultFetcher m α
walkVisitorTThroughCheckpoint = go mempty Seq.empty
  where
    go accum context =
        (VisitorTResultFetcher
         .
         fmap (\x → case x of
            (maybe_solution,Nothing) → Just
                (maybe id (flip mappend) maybe_solution accum
                ,Explored
                ,VisitorTResultFetcher (return Nothing)
                )
            (maybe_solution,Just (new_context, new_unexplored_checkpoint, new_visitor)) → Just $
              let new_accum = maybe id (flip mappend) maybe_solution accum in
                (new_accum
                ,checkpointFromContext new_context new_unexplored_checkpoint
                ,go new_accum new_context new_unexplored_checkpoint new_visitor
                )
         )
        )
        .*
        stepVisitorTThroughCheckpoint context
-- }}}

-- }}}
