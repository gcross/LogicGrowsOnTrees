-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1191: * @file Control/Monad/Trans/Visitor/Checkpoint.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1192: ** << Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Checkpoint where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1193: ** << Import needed modules >>
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join)
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)
import Data.Composition
import Data.Functor.Identity (Identity,runIdentity)
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(),decode,encode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923120247.1194: ** Types
-- @+node:gcross.20110923120247.1195: *3* VisitorCheckpoint
data VisitorCheckpoint =
    CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Explored
  | Unexplored
  deriving (Eq,Read,Show)
-- @+node:gcross.20111020151748.1287: *3* VisitorCheckpointCursor
type VisitorCheckpointCursor = Seq VisitorCheckpointDifferential
-- @+node:gcross.20111020151748.1286: *3* VisitorCheckpointDifferential
data VisitorCheckpointDifferential =
    CacheCheckpointD ByteString
  | ChoiceCheckpointD Branch VisitorCheckpoint
  deriving (Eq,Read,Show)
-- @+node:gcross.20110923164140.1178: *3* VisitorTContext
type VisitorTContext m α = Seq (VisitorTContextStep m α)
type VisitorContext α = VisitorTContext Identity α
-- @+node:gcross.20110923120247.1196: *3* VisitorTContextStep
data VisitorTContextStep m α =
    BranchContextStep Branch
  | CacheContextStep ByteString
  | LeftChoiceContextStep VisitorCheckpoint (VisitorT m α)

type VisitorContextStep = VisitorTContextStep Identity
-- @+node:gcross.20110923164140.1238: *3* VisitorTContextUpdate
type VisitorTContextUpdate m α =
    VisitorTContext m α →
    Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α)

type VisitorContextUpdate α = VisitorTContextUpdate Identity α
-- @+node:gcross.20111019113757.1408: *3* VisitorTResultFetcher
newtype VisitorTResultFetcher m α = VisitorTResultFetcher
    {   fetchVisitorTResult :: m (Maybe (Maybe (VisitorSolution α), VisitorCheckpoint, VisitorTResultFetcher m α))
    }

type VisitorResultFetcher = VisitorTResultFetcher Identity
-- @+node:gcross.20110923164140.1179: ** Functions
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
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorTContext m α → VisitorCheckpoint → VisitorCheckpoint
checkpointFromContext = checkpointFromSequence $
    \step → case step of
        BranchContextStep LeftBranch → flip ChoiceCheckpoint Explored
        BranchContextStep RightBranch → ChoiceCheckpoint Explored
        CacheContextStep cache → CacheCheckpoint cache
        LeftChoiceContextStep right_checkpoint _ → flip ChoiceCheckpoint right_checkpoint
-- @+node:gcross.20111020182554.1265: *3* checkpointFromCursor
checkpointFromCursor :: VisitorCheckpointCursor → VisitorCheckpoint → VisitorCheckpoint
checkpointFromCursor = checkpointFromSequence $
    \step → case step of
        CacheCheckpointD cache → CacheCheckpoint cache
        ChoiceCheckpointD LeftBranch right_checkpoint → flip ChoiceCheckpoint right_checkpoint
        ChoiceCheckpointD RightBranch left_checkpoint → ChoiceCheckpoint left_checkpoint
-- @+node:gcross.20111020182554.1267: *3* checkpointFromSequence
checkpointFromSequence ::
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
-- @+node:gcross.20111019113757.1412: *3* labelFromContext
labelFromContext :: VisitorTContext m α → VisitorLabel
labelFromContext = flip applyContextToLabel rootLabel
-- @+node:gcross.20111020182554.1266: *3* mergeCheckpointRoot
mergeCheckpointRoot :: VisitorCheckpoint → VisitorCheckpoint
mergeCheckpointRoot (ChoiceCheckpoint Unexplored Unexplored) = Unexplored
mergeCheckpointRoot (ChoiceCheckpoint Explored Explored) = Explored
mergeCheckpointRoot (CacheCheckpoint _ Explored) = Explored
mergeCheckpointRoot checkpoint = checkpoint
-- @+node:gcross.20111020182554.1281: *3* moveDownContext
moveDownContext ::
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
-- @+node:gcross.20110923164140.1239: *3* moveUpContext
moveUpContext :: VisitorTContextUpdate m α
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> BranchContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> CacheContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> LeftChoiceContextStep right_checkpoint right_visitor) =
    Just (rest_context |> BranchContextStep RightBranch
         ,right_checkpoint
         ,right_visitor
         )
-- @+node:gcross.20110923164140.1265: *3* pathFromContext
pathFromContext :: VisitorTContext m α → VisitorPath
pathFromContext = fmap pathStepFromContextStep
-- @+node:gcross.20111020182554.1273: *3* pathFromCursor
pathFromCursor :: VisitorCheckpointCursor → VisitorPath
pathFromCursor = fmap pathStepFromCursorDifferential
-- @+node:gcross.20110923164140.1266: *3* pathStepFromContextStep
pathStepFromContextStep :: VisitorTContextStep m α → VisitorStep
pathStepFromContextStep (BranchContextStep active_branch) = ChoiceStep active_branch
pathStepFromContextStep (CacheContextStep cache) = CacheStep cache
pathStepFromContextStep (LeftChoiceContextStep _ _) = ChoiceStep LeftBranch
-- @+node:gcross.20111020182554.1271: *3* pathStepFromCursorDifferential
pathStepFromCursorDifferential :: VisitorCheckpointDifferential → VisitorStep
pathStepFromCursorDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromCursorDifferential (ChoiceCheckpointD active_branch _) = ChoiceStep active_branch
-- @+node:gcross.20110923164140.1246: *3* runVisitorThroughCheckpoint
runVisitorThroughCheckpoint ::
    VisitorCheckpoint →
    Visitor α →
    [(Maybe (VisitorSolution α),VisitorCheckpoint)]
runVisitorThroughCheckpoint checkpoint visitor = go (runVisitorTThroughCheckpoint checkpoint visitor)
  where
    go (runIdentity . fetchVisitorTResult → Nothing) = []
    go (runIdentity . fetchVisitorTResult → Just (maybe_solution,checkpoint,next_result)) =
      (maybe_solution,checkpoint)
      :
      go next_result
-- @+node:gcross.20110923164140.1244: *3* runVisitorTThroughCheckpoint
runVisitorTThroughCheckpoint ::
    (Functor m, Monad m) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTThroughCheckpoint = go Seq.empty
  where
    go context =
        (VisitorTResultFetcher
         .
         fmap (\x → case x of
            (Nothing,Nothing) → Nothing
            (Just solution,Nothing) → Just
                (Just (labelSolution solution)
                ,Explored
                ,VisitorTResultFetcher (return Nothing)
                )
            (maybe_solution,Just (new_context, new_unexplored_checkpoint, new_visitor)) → Just
                (fmap labelSolution maybe_solution
                ,checkpointFromContext new_context new_unexplored_checkpoint
                ,go new_context new_unexplored_checkpoint new_visitor
                )
         )
        )
        .*
        stepVisitorTThroughCheckpoint context
      where
        labelSolution = VisitorSolution (labelFromContext context)
-- @+node:gcross.20111028170027.1294: *3* runVisitorTWithCheckpointing
runVisitorTTWithCheckpointing ::
    (Functor m, Monad m) ⇒
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTTWithCheckpointing = runVisitorTThroughCheckpoint Unexplored
-- @+node:gcross.20111028170027.1292: *3* runVisitorWithCheckpointing
runVisitorWithCheckpointing ::
    Visitor α →
    [(Maybe (VisitorSolution α),VisitorCheckpoint)]
runVisitorWithCheckpointing = runVisitorThroughCheckpoint Unexplored
-- @+node:gcross.20110923164140.1242: *3* stepVisitorThroughCheckpoint
stepVisitorThroughCheckpoint ::
    VisitorContext α →
    VisitorCheckpoint →
    Visitor α →
    (Maybe α,Maybe (VisitorContext α, VisitorCheckpoint, Visitor α))
stepVisitorThroughCheckpoint context checkpoint = runIdentity . stepVisitorTThroughCheckpoint context checkpoint
-- @+node:gcross.20110923164140.1186: *3* stepVisitorTThroughCheckpoint
stepVisitorTThroughCheckpoint ::
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
        mx >>= \x → return . (Nothing,) $
        moveDownMyContext
            (CacheContextStep (encode x))
            Unexplored
            (VisitorT . k $ x)
    (Choice left right :>>= k, ChoiceCheckpoint left_checkpoint right_checkpoint) → return
        (Nothing
        ,moveDownMyContext
            (LeftChoiceContextStep right_checkpoint (right >>= VisitorT . k))
            left_checkpoint
            (left >>= VisitorT . k)
        )
    (Choice left right :>>= k,Unexplored) → return
        (Nothing
        ,moveDownMyContext
            (LeftChoiceContextStep Unexplored (right >>= VisitorT . k))
            Unexplored
            (left >>= VisitorT . k)
        )
    _ → throw PastVisitorIsInconsistentWithPresentVisitor
  where
    moveUpMyContext = moveUpContext context
    moveDownMyContext step checkpoint visitor = moveDownContext step checkpoint visitor context
-- @-others
-- @-leo
