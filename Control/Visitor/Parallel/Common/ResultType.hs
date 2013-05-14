-- Language extensions {{{
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Common.ResultType where

import Data.Monoid

import Control.Visitor.Checkpoint (RunProgress,SearchProgress)

data RunResult
data SearchResult

data ResultType result_kind result where -- {{{
    RunResult :: Monoid result ⇒ ResultType RunResult result
    SearchResult :: ResultType SearchResult result
-- }}}

type family IntermediateTypeOf result_kind result
type instance IntermediateTypeOf RunResult result = result
type instance IntermediateTypeOf SearchResult result = ()

type family FinalTypeOf result_kind result
type instance FinalTypeOf RunResult result = result
type instance FinalTypeOf SearchResult result = Maybe result

type family FinalProgressTypeOf result_kind result
type instance FinalProgressTypeOf RunResult result = RunProgress result
type instance FinalProgressTypeOf SearchResult result = SearchProgress result

initialIntermediateOf :: ResultType result_kind result → IntermediateTypeOf result_kind result -- {{{
initialIntermediateOf RunResult = mempty
initialIntermediateOf SearchResult = ()
{-# INLINE initialIntermediateOf #-}
-- }}}

