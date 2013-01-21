-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Data.Functor
import Data.Monoid
import System.Environment

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Examples.Queens
-- }}}

main =
    getSum . runVisitor . nqueensCount . read . head <$> getArgs
    >>=
    print