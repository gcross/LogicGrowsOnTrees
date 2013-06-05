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

withProofThatProgressIsMonoid :: -- {{{
    VisitorMode visitor_mode →
    (Monoid (ProgressFor visitor_mode) ⇒ α) →
    α
withProofThatProgressIsMonoid AllMode x = x
withProofThatProgressIsMonoid FirstMode x = x
-- }}}
-- }}}
