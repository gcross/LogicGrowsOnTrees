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
data FoundMode result final_result

data VisitorMode visitor_mode where -- {{{
    AllMode :: Monoid result ⇒ VisitorMode (AllMode result)
    FirstMode :: VisitorMode (FirstMode result)
    FoundMode :: Monoid result ⇒ (result → Maybe final_result) → VisitorMode (FoundMode result final_result)
-- }}}
-- }}}

-- Type families {{{
type family ResultFor visitor_mode :: * -- {{{
type instance ResultFor (AllMode result) = result
type instance ResultFor (FirstMode result) = result
type instance ResultFor (FoundMode result final_result) = result
-- }}}

type family ProgressFor visitor_mode :: * -- {{{
type instance ProgressFor (AllMode result) = Progress result
type instance ProgressFor (FirstMode result) = Checkpoint
type instance ProgressFor (FoundMode result final_result) = Progress result
-- }}}

type family FinalResultFor visitor_mode :: * -- {{{
type instance FinalResultFor (AllMode result) = result
type instance FinalResultFor (FirstMode result) = Maybe (Progress result)
type instance FinalResultFor (FoundMode result final_result) = Either result (Progress (final_result,result))
-- }}}

type family WorkerIntermediateValueFor visitor_mode :: * -- {{{
type instance WorkerIntermediateValueFor (AllMode result) = result
type instance WorkerIntermediateValueFor (FirstMode result) = ()
type instance WorkerIntermediateValueFor (FoundMode result final_result) = result
-- }}}

type family WorkerFinalProgressFor visitor_mode :: * -- {{{
type instance WorkerFinalProgressFor (AllMode result) = Progress result
type instance WorkerFinalProgressFor (FirstMode result) = Progress (Maybe result)
type instance WorkerFinalProgressFor (FoundMode result final_result) = Progress (Either result final_result)
-- }}}
-- }}}

-- Functions {{{
checkpointFromIntermediateProgress :: -- {{{
    VisitorMode visitor_mode →
    ProgressFor visitor_mode →
    Checkpoint
checkpointFromIntermediateProgress AllMode = progressCheckpoint
checkpointFromIntermediateProgress FirstMode = id
checkpointFromIntermediateProgress (FoundMode _) = progressCheckpoint
-- }}}

initialProgress :: VisitorMode visitor_mode → ProgressFor visitor_mode -- {{{
initialProgress visitor_mode = withProofThatProgressIsMonoid visitor_mode mempty
-- }}}

initialWorkerIntermediateValue :: -- {{{
    VisitorMode visitor_mode →
    WorkerIntermediateValueFor visitor_mode
initialWorkerIntermediateValue AllMode = mempty
initialWorkerIntermediateValue FirstMode = ()
initialWorkerIntermediateValue (FoundMode _) = mempty
-- }}}

withProofThatProgressIsMonoid :: -- {{{
    VisitorMode visitor_mode →
    (Monoid (ProgressFor visitor_mode) ⇒ α) →
    α
withProofThatProgressIsMonoid AllMode x = x
withProofThatProgressIsMonoid FirstMode x = x
withProofThatProgressIsMonoid (FoundMode _) x = x
-- }}}
-- }}}
