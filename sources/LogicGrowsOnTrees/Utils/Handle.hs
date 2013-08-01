{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains a couple of utility functions for sending an receiving
   'Serialize'-able data over a handle.  They assume a protocol where first a
   line is sent with the size of the data in plain text terminated by a newline,
   and then the raw data is serialized, sent over the line, and deserialize on
   the other side.
 -}
module LogicGrowsOnTrees.Utils.Handle
    (
    -- * Exceptions
      ConnectionLost(..)
    -- * Functions
    , filterEOFExceptions
    , receive
    , send
    ) where

import Prelude hiding (catch)

import Control.Exception (Exception,catch,throwIO)

import qualified Data.ByteString as BS
import Data.ByteString (hGet,hPut)
import Data.Serialize (Serialize,encode,decode)
import Data.Typeable (Typeable)

import System.IO (Handle,hFlush,hGetLine,hPrint)
import System.IO.Error (isEOFError)

--------------------------------------------------------------------------------
---------------------------------- Exceptions ----------------------------------
--------------------------------------------------------------------------------

{-| This connection is thrown when the connection has been lost. -}
data ConnectionLost = ConnectionLost
  deriving (Show,Typeable)
instance Exception ConnectionLost

{-| Replaces EOF IOExceptions with the ConnectionLost exception. -}
filterEOFExceptions = flip catch $
    \e → if isEOFError e then throwIO ConnectionLost else throwIO e

{-| Receives a 'Serialize'-able value from a handle.

    Specifically, this function reads a line with the size of the raw data to be
    receieved in plain text (followed by a newline), reads that much data in
    bytes into a 'ByteString', and then deserializes the 'ByteString' to produce
    the resulting value.

    If the connection has been lost, it throws 'ConnectionLost'.
 -}
receive :: Serialize α ⇒ Handle → IO α
receive handle = filterEOFExceptions $
    hGetLine handle >>= fmap (either error id . decode) . hGet handle . read

{-| Sends a 'Serialize'-able value to a handle.

    Specifically, this function serializes the given value to a 'ByteString',
    and then writes the size of the serialized data in bytes in plaintext
    followed by a newline and then the raw data itself.
 -}
send :: Serialize α ⇒ Handle → α → IO ()
send handle value = filterEOFExceptions $ do
    let encoded_value = encode value
    hPrint handle . BS.length $ encoded_value
    hPut handle encoded_value
    hFlush handle
