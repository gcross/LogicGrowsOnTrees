-- Language extensions {{{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.VisitorMode where

-- Imports {{{
import Data.Monoid

import Control.Visitor.Checkpoint
-- }}}

data VisitorMode r iv ip fv fp where -- {{{
    AllMode :: Monoid r ⇒ VisitorMode r r (Progress r) r (Progress r)
    FirstMode :: VisitorMode r () Checkpoint (Maybe r) (Either Checkpoint r)
-- }}}

checkpointFromFinalProgress :: -- {{{
    VisitorMode r iv ip fv fp →
    fp →
    Checkpoint
checkpointFromFinalProgress AllMode = progressCheckpoint
checkpointFromFinalProgress FirstMode = either id (const Explored)
--}}}

checkpointFromIntermediateProgress :: -- {{{
    VisitorMode r iv ip fv fp →
    ip →
    Checkpoint
checkpointFromIntermediateProgress AllMode = progressCheckpoint
checkpointFromIntermediateProgress FirstMode = id
--}}}

initialIntermediateProgressOf :: VisitorMode r iv ip fv fp → ip -- {{{
initialIntermediateProgressOf AllMode = mempty
initialIntermediateProgressOf FirstMode = mempty
{-# INLINE initialIntermediateProgressOf #-}
-- }}}

initialIntermediateValueOf :: VisitorMode r iv ip fv fp → iv -- {{{
initialIntermediateValueOf AllMode = mempty
initialIntermediateValueOf FirstMode = ()
{-# INLINE initialIntermediateValueOf #-}
-- }}}
