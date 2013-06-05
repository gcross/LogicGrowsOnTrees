-- Language extensions {{{
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.VisitorMode where

-- Imports {{{
import Control.Monad
import Data.Monoid

import Control.Visitor.Checkpoint
-- }}}

-- Classes {{{
class HasVisitorMode (monad :: * → *) where -- {{{
    type VisitorModeFor monad :: *
-- }}}
-- }}}

-- Types {{{
data AllMode result
data FirstMode result

data VisitorMode visitor_mode where -- {{{
    AllMode :: Monoid result ⇒ VisitorMode (AllMode result)
    FirstMode :: VisitorMode (FirstMode result)
-- }}}
-- }}}

-- Type families {{{
type family ResultFor visitor_mode :: * -- {{{
type instance ResultFor (AllMode result) = result
type instance ResultFor (FirstMode result) = result
-- }}}

type family ProgressFor visitor_mode :: * -- {{{
type instance ProgressFor (AllMode result) = Progress result
type instance ProgressFor (FirstMode result) = Checkpoint
-- }}}

type family FinalResultFor visitor_mode :: * -- {{{
type instance FinalResultFor (AllMode result) = result
type instance FinalResultFor (FirstMode result) = Maybe result
-- }}}

type family WorkerIntermediateValueFor visitor_mode :: * -- {{{
type instance WorkerIntermediateValueFor (AllMode result) = result
type instance WorkerIntermediateValueFor (FirstMode result) = ()
-- }}}

type family WorkerFinalProgressFor visitor_mode :: * -- {{{
type instance WorkerFinalProgressFor (AllMode result) = Progress result
type instance WorkerFinalProgressFor (FirstMode result) = Either Checkpoint result
-- }}}
-- }}}

-- Functions {{{
checkpointFromIntermediateProgress :: -- {{{
    VisitorMode visitor_mode →
    ProgressFor visitor_mode →
    Checkpoint
checkpointFromIntermediateProgress AllMode = progressCheckpoint
checkpointFromIntermediateProgress FirstMode = id
-- }}}

constructWorkerFinishedProgress :: -- {{{
    VisitorMode visitor_mode →
    WorkerIntermediateValueFor visitor_mode →
    Maybe (ResultFor visitor_mode) →
    Checkpoint →
    WorkerFinalProgressFor visitor_mode
constructWorkerFinishedProgress AllMode intermediate_result maybe_new_solution explored_checkpoint =
    Progress
        explored_checkpoint
        (maybe intermediate_result (mappend intermediate_result) maybe_new_solution)
constructWorkerFinishedProgress FirstMode _ maybe_new_result explored_checkpoint =
    maybe (Left explored_checkpoint) Right maybe_new_result
-- }}}

extractFinalValueFromFinalProgress :: -- {{{
    VisitorMode visitor_mode →
    WorkerFinalProgressFor visitor_mode →
    FinalResultFor visitor_mode
extractFinalValueFromFinalProgress AllMode = progressResult
extractFinalValueFromFinalProgress FirstMode = either (const Nothing) Just
-- }}}

initialProgress :: VisitorMode visitor_mode → ProgressFor visitor_mode -- {{{
initialProgress visitor_mode = withProofThatProgressIsMonoid visitor_mode mempty
-- }}}

initialWorkerIntermediateValue :: -- {{{
    VisitorMode visitor_mode →
    WorkerIntermediateValueFor visitor_mode
initialWorkerIntermediateValue AllMode = mempty
initialWorkerIntermediateValue FirstMode = ()
-- }}}

progressFrom :: -- {{{
    VisitorMode visitor_mode →
    WorkerIntermediateValueFor visitor_mode →
    Checkpoint →
    ProgressFor visitor_mode
progressFrom AllMode = flip Progress
progressFrom FirstMode = const id
-- }}}

reactToFinalProgress :: -- {{{
    Monad m ⇒
    VisitorMode visitor_mode →
    (FinalResultFor visitor_mode → m (Checkpoint,FinalResultFor visitor_mode)) →
    (ProgressFor visitor_mode → m (ProgressFor visitor_mode)) →
    WorkerFinalProgressFor visitor_mode →
    m (Checkpoint,FinalResultFor visitor_mode)
reactToFinalProgress AllMode _ updateAndReturnProgress final_progress = do
    Progress checkpoint new_results ← updateAndReturnProgress final_progress
    return (checkpoint,new_results)
reactToFinalProgress FirstMode finishWithResult updateAndReturnProgress final_progress =
    either
        (liftM (,Nothing) . updateAndReturnProgress)
        (finishWithResult . Just)
    $
    final_progress
-- }}}

withProofThatProgressIsMonoid :: -- {{{
    VisitorMode visitor_mode →
    (Monoid (ProgressFor visitor_mode) ⇒ α) →
    α
withProofThatProgressIsMonoid AllMode x = x
withProofThatProgressIsMonoid FirstMode x = x
-- }}}
-- }}}
