-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Utils.Handle
    ( receive
    , send
    ) where

-- Imports {{{
import qualified Data.ByteString as BS
import Data.ByteString (hGet,hPut)
import Data.Serialize (Serialize,encode,decode)

import System.IO (Handle,hFlush,hGetLine,hPrint)
-- }}}

-- Functions {{{

receive :: Serialize α ⇒ Handle → IO α -- {{{
receive handle = hGetLine handle >>= fmap (either error id . decode) . hGet handle . read
-- }}}

send :: Serialize α ⇒ Handle → α → IO () -- {{{
send handle value = do
    let encoded_value = encode value
    hPrint handle . BS.length $ encoded_value
    hPut handle encoded_value
    hFlush handle
-- }}}

-- }}}
