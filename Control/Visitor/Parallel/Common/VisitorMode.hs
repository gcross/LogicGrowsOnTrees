-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.VisitorMode where

-- Imports {{{
import Control.Monad
import Data.Monoid
import GHC.Exts (Constraint)

import Control.Visitor.Checkpoint
-- }}}

-- Classes {{{
class Monoid (ProgressFor visitor_mode) ⇒ VisitorMode visitor_mode where -- {{{
    type ResultFor visitor_mode :: *

    type ProgressFor visitor_mode :: *
    type FinalResultFor visitor_mode :: *

    type WorkerIntermediateValueFor visitor_mode :: *
    type WorkerFinalProgressFor visitor_mode :: *

    checkpointFromIntermediateProgress ::
        visitor_mode →
        ProgressFor visitor_mode →
        Checkpoint

    constructWorkerFinishedProgress ::
        visitor_mode →
        WorkerIntermediateValueFor visitor_mode →
        Maybe (ResultFor visitor_mode) →
        Checkpoint →
        WorkerFinalProgressFor visitor_mode

    extractFinalValueFromFinalProgress ::
        visitor_mode →
        WorkerFinalProgressFor visitor_mode →
        FinalResultFor visitor_mode

    initialWorkerIntermediateValue ::
        visitor_mode →
        WorkerIntermediateValueFor visitor_mode

    progressFrom ::
        visitor_mode →
        WorkerIntermediateValueFor visitor_mode →
        Checkpoint →
        ProgressFor visitor_mode

    reactToFinalProgress ::
        Monad m ⇒
        visitor_mode →
        (FinalResultFor visitor_mode → m (Checkpoint,FinalResultFor visitor_mode)) →
        (ProgressFor visitor_mode → m (ProgressFor visitor_mode)) →
        WorkerFinalProgressFor visitor_mode →
        m (Checkpoint,FinalResultFor visitor_mode)

    reactToSolutionFound ::
        Monad m ⇒
        visitor_mode →
        WorkerIntermediateValueFor visitor_mode →
        (ResultFor visitor_mode) →
        (WorkerIntermediateValueFor visitor_mode → m α) →
        (WorkerFinalProgressFor visitor_mode → m α) →
        m α
-- }}}
class HasVisitorMode (monad :: * → *) where -- {{{
    type VisitorModeFor monad :: *
-- }}}
-- }}}

-- Visitor modes {{{
data AllMode result = AllMode -- {{{
instance Monoid result ⇒ VisitorMode (AllMode result) where
    type ResultFor (AllMode result) = result

    type ProgressFor (AllMode result) = Progress result
    type FinalResultFor (AllMode result) = result

    type WorkerIntermediateValueFor (AllMode result) = result
    type WorkerFinalProgressFor (AllMode result) = Progress result

    checkpointFromIntermediateProgress _ = progressCheckpoint

    constructWorkerFinishedProgress _ intermediate_result maybe_new_solution explored_checkpoint =
        Progress
            explored_checkpoint
            (maybe intermediate_result (mappend intermediate_result) maybe_new_solution)

    extractFinalValueFromFinalProgress _ = progressResult

    initialWorkerIntermediateValue _ = mempty

    progressFrom _ = flip Progress

    reactToFinalProgress _ _ updateAndReturnProgress final_progress = do
        Progress checkpoint new_results ← updateAndReturnProgress final_progress
        return (checkpoint,new_results)

    reactToSolutionFound _ intermediate_result result loopWithNewIntermediate _ =
        loopWithNewIntermediate (intermediate_result <> result)
-- }}}
data FirstMode result = FirstMode -- {{{
instance VisitorMode (FirstMode result) where
    type ResultFor (FirstMode result) = result

    type ProgressFor (FirstMode result) = Checkpoint
    type FinalResultFor (FirstMode result) = Maybe result

    type WorkerIntermediateValueFor (FirstMode result) = ()
    type WorkerFinalProgressFor (FirstMode result) = Either Checkpoint result

    checkpointFromIntermediateProgress _ = id

    constructWorkerFinishedProgress _ _ maybe_new_result explored_checkpoint =
        maybe (Left explored_checkpoint) Right maybe_new_result

    extractFinalValueFromFinalProgress _ = either (const Nothing) Just

    initialWorkerIntermediateValue _ = ()

    progressFrom _ = const id

    reactToFinalProgress _ finishWithResult updateAndReturnProgress =
        either
            (liftM (,Nothing) . updateAndReturnProgress)
            (finishWithResult . Just)

    reactToSolutionFound _ _ solution _ finish = finish (Right solution)
-- }}}
-- }}}
