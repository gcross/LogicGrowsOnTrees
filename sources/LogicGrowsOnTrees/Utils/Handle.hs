{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains a couple of utility functions for sending and receiving
    'Serialize'-able data over a handle. Because the size of the serialized
    value can depend on the value being sent, these functions employ a protocol
    in which first the size of the serialized data is sent as a 64-bit
    big-endian word, and then the serialized data itself is sent.
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
import Data.Serialize (Serialize,encode,decode,runGet,runPut,getWord64be,putWord64be)
import Data.Typeable (Typeable)

import System.IO (Handle,hFlush)
import System.IO.Error (isEOFError,ioeGetErrorType)

--------------------------------------------------------------------------------
---------------------------------- Exceptions ----------------------------------
--------------------------------------------------------------------------------

{-| This exception is thrown when the connection has been lost. -}
data ConnectionLost = ConnectionLost
  deriving (Show,Typeable)
instance Exception ConnectionLost

{-| Replaces EOF 'IOException's with the 'ConnectionLost' exception. -}
filterEOFExceptions = flip catch $
    \e → if isEOFError e || (show . ioeGetErrorType $ e) == "resource vanished"
        then throwIO ConnectionLost
        else throwIO e

{-| Receives a 'Serialize'-able value from a handle.

    Specifically, this function reads in a 64-bit big-endian word with the
    size of the raw data to be read, reads that much data in bytes into a
    'ByteString', and then deserializes the 'ByteString' to produce the
    resulting value.

    If the connection has been lost, it throws 'ConnectionLost'.
 -}
receive :: Serialize α ⇒ Handle → IO α
receive handle = filterEOFExceptions $
    fmap (either error id . decode)
    .
    hGet handle
    .
    either error fromIntegral
    .
    runGet getWord64be
    =<<
    hGet handle 8

{-| Sends a 'Serialize'-able value to a handle.

    Specifically, this function serializes the given value to a 'ByteString',
    and then writes the size of the serialized data in bytes as a 64-bit
    big-endian word followed by the raw data itself.

    If the connection has been lost, it throws 'ConnectionLost'.
 -}
send :: Serialize α ⇒ Handle → α → IO ()
send handle value = filterEOFExceptions $ do
    let encoded_value = encode value
    hPut handle . runPut . putWord64be . fromIntegral . BS.length $ encoded_value
    hPut handle encoded_value
    hFlush handle
