-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Data.Functor
import Data.Monoid
import Data.Serialize (Serialize(..))

import Options.Applicative

import System.Environment

import Control.Monad.Trans.Visitor.Main
import Control.Monad.Trans.Visitor.Parallel.Processes
import Control.Monad.Trans.Visitor.Examples.Queens
-- }}}

instance Serialize (Sum Int) where
    put = put . getSum
    get = fmap Sum get

main =
    mainVisitor
        driver
        (argument auto
            (   metavar "#"
             <> help "board size"
            )
        )
        mempty
        (\_ termination_reason → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (Sum count) → print count
                Failure message → error $ "error: " ++ message
        )
        nqueensCount
