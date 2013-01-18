-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Data.Functor
import Data.List (sort)
import System.Environment

import Control.Monad.Trans.Visitor.Examples.Queens
-- }}}

main =
    sort . nqueensSolutions . read . head <$> getArgs
    >>=
    mapM_ print