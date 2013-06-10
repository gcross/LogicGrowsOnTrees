{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains a couple of utility functions for sending an receiving
   'Serialize'-able data over a handle.  They assume a protocol where first a
   line is sent with the size of the data in plain text terminated by a newline,
   and then the raw data is serialized, sent over the line, and deserialize on
   the other side.
 -}
module Visitor.Utils.Handle
    ( receive
    , send
    ) where

import qualified Data.ByteString as BS
import Data.ByteString (hGet,hPut)
import Data.Serialize (Serialize,encode,decode)

import System.IO (Handle,hFlush,hGetLine,hPrint)

{-| Receives a 'Serialize'-able value from a handle.

    Specifically, this function reads a line with the size of the raw data to be
    receieved in plain text (followed by a newline), reads that much data in
    bytes into a 'ByteString', and then deserializes the 'ByteString' to produce
    the resulting value.
 -}
receive :: Serialize α ⇒ Handle → IO α
receive handle = hGetLine handle >>= fmap (either error id . decode) . hGet handle . read

{-| Sends a 'Serialize'-able value to a handle.

    Specifically, this function serializes the given value to a 'ByteString',
    and then writes the size of the serialized data in bytes in plaintext
    followed by a newline and then the raw data itself.
 -}
send :: Serialize α ⇒ Handle → α → IO ()
send handle value = do
    let encoded_value = encode value
    hPrint handle . BS.length $ encoded_value
    hPut handle encoded_value
    hFlush handle



