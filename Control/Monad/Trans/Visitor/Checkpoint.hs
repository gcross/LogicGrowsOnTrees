-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1191: * @file Control/Monad/Trans/Visitor/Checkpoint.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1192: ** << Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Checkpoint where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1193: ** << Import needed modules >>
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join)
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
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
    BranchCheckpoint Bool VisitorCheckpoint
  | CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Unexplored
-- @+node:gcross.20110923120247.1196: *3* VisitorCheckpointDifferential
data VisitorCheckpointDifferential m α =
    BranchCheckpointD Bool
  | CacheCheckpointD ByteString
  | ChoiceCheckpointD Bool VisitorCheckpoint (VisitorT m α)
-- @+node:gcross.20110923164140.1178: *3* VisitorCheckpointContext
type VisitorCheckpointContext m α = Seq (VisitorCheckpointDifferential m α)
-- @+node:gcross.20110923164140.1179: ** Functions
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorCheckpointContext m α → VisitorCheckpoint
checkpointFromContext (viewl → EmptyL) = Unexplored
checkpointFromContext (viewl → differential :< rest) =
    let rest_checkpoint = checkpointFromContext rest
    in case differential of
        BranchCheckpointD which_explored → BranchCheckpoint which_explored rest_checkpoint
        CacheCheckpointD cache → CacheCheckpoint cache rest_checkpoint
        ChoiceCheckpointD False right_checkpoint _ → ChoiceCheckpoint rest_checkpoint right_checkpoint
        ChoiceCheckpointD True left_checkpoint _ → ChoiceCheckpoint left_checkpoint rest_checkpoint
-- @+node:gcross.20110923164140.1186: *3* runCheckpointContext
runVisitorWithCheckpoint ::
    Monad m ⇒
    (forall β. (VisitorCheckpointContext m α → (VisitorCheckpointContext m α,β)) → m β) →
    (α → m ()) →
    VisitorCheckpoint →
    VisitorT m α →
    m ()
runVisitorWithCheckpoint updateCheckpointContext acceptSolution checkpoint = viewT >=> \view → case view of
    Return x → acceptSolution x >> goUpward
    Null :>>= _ → goUpward
    Cache mx :>>= k →
        case checkpoint of
            CacheCheckpoint cache rest_checkpoint →
                goDownward (CacheCheckpointD cache) rest_checkpoint $ either error k (decode cache)
            Unexplored → do
                x ← mx
                goDownward (CacheCheckpointD (encode x)) Unexplored (k x)
            _ → throw ChoiceStepAtCachePoint
    Choice left right :>>= k →
        case checkpoint of
            BranchCheckpoint False right_checkpoint →
                goDownward (BranchCheckpointD False) right_checkpoint (right >>= k)
            BranchCheckpoint True left_checkpoint →
                goDownward (BranchCheckpointD True) left_checkpoint (left >>= k)
            ChoiceCheckpoint left_checkpoint right_checkpoint →
                goDownward (ChoiceCheckpointD False right_checkpoint (right >>= k)) left_checkpoint (left >>= k)
            _ → throw CacheStepAtChoicePoint
  where
    recurse = runVisitorWithCheckpoint updateCheckpointContext acceptSolution

    goUpward = join . updateCheckpointContext $ computeNewContext
      where
        computeNewContext (viewr → EmptyR) = (Seq.empty,return ())
        computeNewContext (viewr → rest_context :> differential) = case differential of
            BranchCheckpointD _ →
                computeNewContext rest_context
            CacheCheckpointD _ →
                computeNewContext rest_context
            ChoiceCheckpointD which_explored other_checkpoint other →
                (rest_context |> BranchCheckpointD which_explored
                ,recurse other_checkpoint other
                )

    goDownward differential checkpoint v = do
        updateCheckpointContext $ (\context → (context |> differential,()))
        recurse checkpoint v
-- @-others
-- @-leo
