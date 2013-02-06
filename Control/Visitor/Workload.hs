-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Workload where

-- Imports {{{
import Control.Monad (join)
import Data.Composition ((.*))
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq
import Data.Serialize

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Label
import Control.Visitor.Path
-- }}}

-- Types {{{

data VisitorWorkload = VisitorWorkload -- {{{
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: Checkpoint
    } deriving (Eq,Show)
$( derive makeSerialize ''VisitorWorkload )
-- }}}

-- }}}

-- Instances {{{
instance Ord VisitorWorkload where
    x `compare` y =
        case (compare `on` workloadDepth) x y of
            EQ → case (compare `on` visitorWorkloadPath) x y of
                EQ → (compare `on` visitorWorkloadCheckpoint) x y
                c → c
            c → c
-- }}}

-- Values {{{

entire_workload = VisitorWorkload Seq.empty Unexplored

-- }}}

-- Functions {{{

runVisitorThroughWorkload :: -- {{{
    Monoid α ⇒
    VisitorWorkload →
    Visitor α →
    α
runVisitorThroughWorkload =
    (fst . last)
    .*
    walkVisitorThroughWorkload
-- }}}

runVisitorTThroughWorkload :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorWorkload →
    VisitorT m α →
    m α
runVisitorTThroughWorkload = gatherResults .* walkVisitorTThroughWorkload
-- }}}

walkVisitorThroughWorkload :: -- {{{
    Monoid α ⇒
    VisitorWorkload →
    Visitor α →
    [(α,Checkpoint)]
walkVisitorThroughWorkload VisitorWorkload{..} =
    walkVisitorThroughCheckpoint visitorWorkloadCheckpoint
    .
    sendVisitorDownPath visitorWorkloadPath
-- }}}

walkVisitorTThroughWorkload :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorWorkload →
    VisitorT m α →
    ResultFetcher m α
walkVisitorTThroughWorkload VisitorWorkload{..} =
    ResultFetcher
    .
    join
    .
    fmap (
        fetchResult
        .
        walkVisitorTThroughCheckpoint visitorWorkloadCheckpoint
    )
    .
    sendVisitorTDownPath visitorWorkloadPath
-- }}}

workloadDepth :: VisitorWorkload → Int -- {{{
workloadDepth = Seq.length . visitorWorkloadPath -- }}}

-- }}}
