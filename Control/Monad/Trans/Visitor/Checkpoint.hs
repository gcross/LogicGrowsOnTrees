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
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923120247.1194: ** Types
-- @+node:gcross.20110923120247.1195: *3* VisitorCheckpoint
data VisitorCheckpoint =
    BranchCheckpoint WhichBranchActive VisitorCheckpoint
  | CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Unexplored
  deriving (Eq,Read,Show)
-- @+node:gcross.20110923120247.1196: *3* VisitorTCheckpointDifferential
data VisitorTCheckpointDifferential m α =
    BranchCheckpointD WhichBranchActive
  | CacheCheckpointD ByteString
  | ChoiceCheckpointD WhichBranchActive VisitorCheckpoint (VisitorT m α)

type VisitorCheckpointDifferential = VisitorTCheckpointDifferential Identity
-- @+node:gcross.20110923164140.1178: *3* VisitorTCheckpointContext
type VisitorTCheckpointContext m α = Seq (VisitorTCheckpointDifferential m α)
type VisitorCheckpointContext α = VisitorTCheckpointContext Identity α
-- @+node:gcross.20110923164140.1238: *3* VisitorTCheckpointContextMove
data VisitorTCheckpointContextMove m α =
    MoveUpContext
  | MoveDownContext (VisitorTCheckpointDifferential m α) VisitorCheckpoint (VisitorT m α)

type VisitorCheckpointContextMove = VisitorTCheckpointContextMove Identity
-- @+node:gcross.20111019113757.1408: *3* VisitorTResultFetcher
newtype VisitorTResultFetcher m α = VisitorTResultFetcher
    {   fetchVisitorTResult :: m (Maybe (Maybe (VisitorSolution α), VisitorCheckpoint, VisitorTResultFetcher m α))
    }
-- @+node:gcross.20110923164140.1179: ** Functions
-- @+node:gcross.20110923164140.1240: *3* applyContextMove
applyContextMove ::
    VisitorTCheckpointContextMove m α →
    VisitorTCheckpointContext m α →
    Maybe (VisitorTCheckpointContext m α, VisitorCheckpoint, VisitorT m α)
applyContextMove MoveUpContext = moveUpContext
applyContextMove (MoveDownContext differential checkpoint v) =
    Just
    .
    (,checkpoint,v)
    .
    (|> differential)
-- @+node:gcross.20111019113757.1400: *3* applyContextToLabel
applyContextToLabel :: VisitorTCheckpointContext m α → VisitorLabel → VisitorLabel
applyContextToLabel (viewl → EmptyL) = id
applyContextToLabel (viewl → differential :< rest) =
    applyContextToLabel rest
    .
    case differential of
        CacheCheckpointD _ → id
        BranchCheckpointD active_branch → labelTransformerFrom active_branch
        ChoiceCheckpointD active_branch _ _ → labelTransformerFrom active_branch
-- @+node:gcross.20111019113757.1412: *3* labelFromContext
labelFromContext :: VisitorTCheckpointContext m α → VisitorLabel
labelFromContext = flip applyContextToLabel rootLabel
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorCheckpoint → VisitorTCheckpointContext m α → VisitorCheckpoint
checkpointFromContext unexplored_checkpoint (viewl → EmptyL) = unexplored_checkpoint
checkpointFromContext unexplored_checkpoint (viewl → differential :< rest) =
    let rest_checkpoint = checkpointFromContext unexplored_checkpoint rest
    in case differential of
        BranchCheckpointD active_branch → BranchCheckpoint active_branch rest_checkpoint
        CacheCheckpointD cache → CacheCheckpoint cache rest_checkpoint
        ChoiceCheckpointD LeftBranchActive right_checkpoint _ → ChoiceCheckpoint rest_checkpoint right_checkpoint
        ChoiceCheckpointD RightBranchActive left_checkpoint _ → ChoiceCheckpoint left_checkpoint rest_checkpoint
-- @+node:gcross.20110923164140.1239: *3* moveUpContext
moveUpContext :: VisitorTCheckpointContext m α → Maybe (VisitorTCheckpointContext m α, VisitorCheckpoint, VisitorT m α)
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> BranchCheckpointD _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> CacheCheckpointD _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> ChoiceCheckpointD branch_explored other_checkpoint other) =
    Just (rest_context |> BranchCheckpointD (oppositeBranchOf branch_explored)
         ,other_checkpoint
         ,other
         )
-- @+node:gcross.20110923164140.1265: *3* pathFromContext
pathFromContext :: VisitorTCheckpointContext m α → VisitorPath
pathFromContext = fmap pathStepFromDifferential
-- @+node:gcross.20110923164140.1266: *3* pathStepFromDifferential
pathStepFromDifferential :: VisitorTCheckpointDifferential m α → VisitorStep
pathStepFromDifferential (BranchCheckpointD active_branch) = ChoiceStep active_branch
pathStepFromDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromDifferential (ChoiceCheckpointD active_branch _ _) = ChoiceStep active_branch
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
         fmap (\(maybe_solution, move) →
            case applyContextMove move context of
                Nothing → Nothing
                Just (new_context, new_unexplored_checkpoint, new_visitor) → Just
                    (fmap (
                        VisitorSolution
                        .
                        labelFromContext
                        $
                        context
                     ) maybe_solution
                    ,checkpointFromContext new_unexplored_checkpoint new_context
                    ,go new_context new_unexplored_checkpoint new_visitor
                    )
         )

        )
        .*
        stepVisitorTThroughCheckpoint
-- @+node:gcross.20110923164140.1242: *3* stepVisitorThroughCheckpoint
stepVisitorThroughCheckpoint ::
    VisitorCheckpoint →
    Visitor α →
    (Maybe α, VisitorCheckpointContextMove α)
stepVisitorThroughCheckpoint checkpoint = runIdentity . stepVisitorTThroughCheckpoint checkpoint
-- @+node:gcross.20110923164140.1186: *3* stepVisitorTThroughCheckpoint
stepVisitorTThroughCheckpoint ::
    Monad m ⇒
    VisitorCheckpoint →
    VisitorT m α →
    m (Maybe α, VisitorTCheckpointContextMove m α)
stepVisitorTThroughCheckpoint checkpoint = viewT . unwrapVisitorT >=> \view →  case view of
    Return x → return (Just x, MoveUpContext)
    Null :>>= _ → return (Nothing, MoveUpContext)
    Cache mx :>>= k →
        case checkpoint of
            CacheCheckpoint cache rest_checkpoint → return
                (Nothing
                ,MoveDownContext (CacheCheckpointD cache) rest_checkpoint $ either error (VisitorT . k) (decode cache)
                )
            Unexplored → do
                x ← mx
                return
                    (Nothing
                    ,MoveDownContext (CacheCheckpointD (encode x)) Unexplored ((VisitorT . k) x)
                    )
            _ → throw ChoiceStepAtCachePoint
    Choice left right :>>= k → return 
        (Nothing
        ,case checkpoint of
            BranchCheckpoint LeftBranchActive left_checkpoint →
                MoveDownContext (BranchCheckpointD LeftBranchActive) left_checkpoint (left >>= VisitorT . k)
            BranchCheckpoint RightBranchActive right_checkpoint →
                MoveDownContext (BranchCheckpointD RightBranchActive) right_checkpoint (right >>= VisitorT . k)
            ChoiceCheckpoint left_checkpoint right_checkpoint →
                MoveDownContext (ChoiceCheckpointD LeftBranchActive right_checkpoint (right >>= VisitorT . k)) left_checkpoint (left >>= VisitorT . k)
            Unexplored →
                MoveDownContext (ChoiceCheckpointD LeftBranchActive Unexplored (right >>= VisitorT . k)) Unexplored (left >>= VisitorT . k)
            _ → throw CacheStepAtChoicePoint
        )
-- @-others
-- @-leo
