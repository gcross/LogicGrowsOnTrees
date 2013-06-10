-- Language extensions {{{
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Parallel.Common.VisitorMode where

-- Imports {{{
import Data.Monoid

import Visitor.Checkpoint
-- }}}

-- Classes {{{
class HasVisitorMode (monad :: * → *) where -- {{{
    type VisitorModeFor monad :: *
-- }}}
-- }}}

-- Types {{{
data AllMode result
data FirstMode result
data FoundModeUsingPull result final_result
data FoundModeUsingPush result final_result

data VisitorMode visitor_mode where -- {{{
    AllMode :: Monoid result ⇒ VisitorMode (AllMode result)
    FirstMode :: VisitorMode (FirstMode result)
    FoundModeUsingPull :: Monoid result ⇒ (result → Maybe final_result) → VisitorMode (FoundModeUsingPull result final_result)
    FoundModeUsingPush :: Monoid result ⇒ (result → Maybe final_result) → VisitorMode (FoundModeUsingPush result final_result)
-- }}}
-- }}}

-- Type families {{{
type family ResultFor visitor_mode :: * -- {{{
type instance ResultFor (AllMode result) = result
type instance ResultFor (FirstMode result) = result
type instance ResultFor (FoundModeUsingPull result final_result) = result
type instance ResultFor (FoundModeUsingPush result final_result) = result
-- }}}

type family ProgressFor visitor_mode :: * -- {{{
type instance ProgressFor (AllMode result) = Progress result
type instance ProgressFor (FirstMode result) = Checkpoint
type instance ProgressFor (FoundModeUsingPull result final_result) = Progress result
type instance ProgressFor (FoundModeUsingPush result final_result) = Progress result
-- }}}

type family FinalResultFor visitor_mode :: * -- {{{
type instance FinalResultFor (AllMode result) = result
type instance FinalResultFor (FirstMode result) = Maybe (Progress result)
type instance FinalResultFor (FoundModeUsingPull result final_result) = Either result (Progress (final_result,result))
type instance FinalResultFor (FoundModeUsingPush result final_result) = Either result (Progress final_result)
-- }}}

type family WorkerIntermediateValueFor visitor_mode :: * -- {{{
type instance WorkerIntermediateValueFor (AllMode result) = result
type instance WorkerIntermediateValueFor (FirstMode result) = ()
type instance WorkerIntermediateValueFor (FoundModeUsingPull result final_result) = result
type instance WorkerIntermediateValueFor (FoundModeUsingPush result final_result) = ()
-- }}}

type family WorkerFinalProgressFor visitor_mode :: * -- {{{
type instance WorkerFinalProgressFor (AllMode result) = Progress result
type instance WorkerFinalProgressFor (FirstMode result) = Progress (Maybe result)
type instance WorkerFinalProgressFor (FoundModeUsingPull result final_result) = Progress (Either result final_result)
type instance WorkerFinalProgressFor (FoundModeUsingPush result final_result) = Progress result
-- }}}
-- }}}

-- Functions {{{
checkpointFromIntermediateProgress :: -- {{{
    VisitorMode visitor_mode →
    ProgressFor visitor_mode →
    Checkpoint
checkpointFromIntermediateProgress AllMode = progressCheckpoint
checkpointFromIntermediateProgress FirstMode = id
checkpointFromIntermediateProgress (FoundModeUsingPull _) = progressCheckpoint
checkpointFromIntermediateProgress (FoundModeUsingPush _) = progressCheckpoint
-- }}}

initialProgress :: VisitorMode visitor_mode → ProgressFor visitor_mode -- {{{
initialProgress AllMode = mempty
initialProgress FirstMode = mempty
initialProgress (FoundModeUsingPull _) = mempty
initialProgress (FoundModeUsingPush _) = mempty
-- }}}

initialWorkerIntermediateValue :: -- {{{
    VisitorMode visitor_mode →
    WorkerIntermediateValueFor visitor_mode
initialWorkerIntermediateValue AllMode = mempty
initialWorkerIntermediateValue FirstMode = ()
initialWorkerIntermediateValue (FoundModeUsingPull _) = mempty
initialWorkerIntermediateValue (FoundModeUsingPush _) = ()
-- }}}
-- }}}
