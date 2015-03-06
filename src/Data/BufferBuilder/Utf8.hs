{-# LANGUAGE GeneralizedNewtypeDeriving, MagicHash, BangPatterns #-}

{-|
A library for efficiently building up a buffer of UTF-8-encoded text.  If only
safe functions are used, the resulting 'ByteString' is guaranteed to be valid
UTF-8.

To run a sequence of Utf8Builder actions and retrieve the resulting buffer, use
'runUtf8Builder'.

In special situations, for maximum performance, unsafe functions are
also provided.  The unsafe functions do not guarantee the buffer is
correct UTF-8.

This module is built on top of "Data.BufferBuilder".
-}
module Data.BufferBuilder.Utf8 (

    -- * The Utf8Builder Monad
      Utf8Builder
    , runUtf8Builder

    -- * Text encoding
    , appendText
    , appendString
    , appendChar

    -- * ASCII-7
    , appendByte7
    , appendChar7
    , appendBS7
    , appendLiteral7

    -- * URL percent-encoding
    , appendUrlEncoded

    -- * Printing numbers
    , appendDecimalSignedInt
    , appendDecimalDouble

    -- * Escaped JSON
    , appendEscapedJson
    , appendEscapedJsonLiteral
    , appendEscapedJsonText

    -- * Unsafe append operations
    , unsafeAppendBufferBuilder
    , unsafeAppendByte
    , unsafeAppendChar8
    , unsafeAppendLiteral
    , unsafeAppendLiteralN
    , unsafeAppendBS
    ) where

import GHC.Base
import GHC.Word
import Control.Applicative
import Data.ByteString (ByteString)
import Data.BufferBuilder (BufferBuilder)
import qualified Data.BufferBuilder as BB
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

newtype Utf8Builder a = Utf8Builder { unBuilder :: BufferBuilder a }
    deriving (Functor, Applicative, Monad)

-- | Run a sequence of 'Utf8Builder' actions and extracting the resulting
-- buffer as a 'ByteString'.
runUtf8Builder :: Utf8Builder () -> ByteString
runUtf8Builder a = BB.runBufferBuilder $ unBuilder a
{-# INLINE runUtf8Builder #-}


-- Text encoding

-- TODO: optimize appendText with custom UTF-16 -> UTF-8 encoder in C

-- | Encodes the given 'Text' in UTF-8, appending it to the buffer.
appendText :: Text -> Utf8Builder ()
appendText a = Utf8Builder $ BB.appendBS $ encodeUtf8 a
{-# INLINE appendText #-}

-- | Encodes the given 'String' in UTF-8, appending it to the buffer.
appendString :: String -> Utf8Builder ()
appendString s = mapM_ appendChar s
{-# INLINABLE appendString #-}

-- | Encodes a single 'Char' in UTF-8, appending it to the buffer.
appendChar :: Char -> Utf8Builder ()
appendChar c = Utf8Builder $ BB.appendCharUtf8 c
{-# INLINE appendChar #-}


-- ASCII-7

-- | Appends the bottom 7 bits of a byte to the buffer.
appendByte7 :: Word8 -> Utf8Builder ()
appendByte7 = Utf8Builder . BB.appendByte7
{-# INLINE appendByte7 #-}

-- | Appends the bottom 7 bits of a 'Char' to the buffer.
appendChar7 :: Char -> Utf8Builder ()
appendChar7 = Utf8Builder . BB.appendChar7
{-# INLINE appendChar7 #-}

-- | Appends the given ByteString to the buffer, taking the bottom
-- 7 bits of each byte.
appendBS7 :: ByteString -> Utf8Builder ()
appendBS7 = Utf8Builder . BB.appendBS7
{-# INLINE appendBS7 #-}

-- | Appends the zero-terminated byte string at the given address
-- to the buffer, taking the bottom 7 bits of each byte.
appendLiteral7 :: Addr# -> Utf8Builder ()
appendLiteral7 addr = Utf8Builder $ BB.appendLiteral7 addr
{-# INLINE appendLiteral7 #-}


-- URL percent-encoding

-- | Directly calls 'BB.appendUrlEncoded'.  The output from URL
-- percent-encoding is guaranteed to be valid UTF-8.
appendUrlEncoded :: ByteString -> Utf8Builder ()
appendUrlEncoded = Utf8Builder . BB.appendUrlEncoded
{-# INLINE appendUrlEncoded #-}


-- Printing numbers

appendDecimalSignedInt :: Int -> Utf8Builder ()
appendDecimalSignedInt a = Utf8Builder $ BB.appendDecimalSignedInt a
{-# INLINE appendDecimalSignedInt #-}

appendDecimalDouble :: Double -> Utf8Builder ()
appendDecimalDouble d = Utf8Builder $ BB.appendDecimalDouble d
{-# INLINE appendDecimalDouble #-}


-- Escaped JSON

appendEscapedJsonLiteral :: Addr# -> Utf8Builder ()
appendEscapedJsonLiteral addr = Utf8Builder $ BB.appendEscapedJsonLiteral addr

appendEscapedJson :: ByteString -> Utf8Builder ()
appendEscapedJson a = Utf8Builder $ BB.appendEscapedJson a
{-# INLINE appendEscapedJson #-}

appendEscapedJsonText :: Text -> Utf8Builder ()
appendEscapedJsonText txt = Utf8Builder $ BB.appendEscapedJsonText txt
{-# INLINE appendEscapedJsonText #-}


-- Unsafe

-- | Directly append a BufferBuilder into the UTF-8 code stream.  Incorrect
-- use of this function can result in invalid UTF-8.
unsafeAppendBufferBuilder :: BufferBuilder () -> Utf8Builder ()
unsafeAppendBufferBuilder = Utf8Builder

-- | Directly append a byte into the UTF-8 code stream.  Incorrect use of
-- this function can result in invalid UTF-8.
unsafeAppendByte :: Word8 -> Utf8Builder ()
unsafeAppendByte = Utf8Builder . BB.appendByte
{-# INLINE unsafeAppendByte #-}

-- | Directly append the bottom 8 bits of the given character to the UTF-8
-- code stream.  Incorrect use of this function can result in invalid UTF-8.
unsafeAppendChar8 :: Char -> Utf8Builder ()
unsafeAppendChar8 = Utf8Builder . BB.appendChar8
{-# INLINE unsafeAppendChar8 #-}

-- | Directly append the zero-terminated byte sequence pointed to by
-- the given address.  Be careful that the referenced byte sequence
-- contains valid UTF-8.
unsafeAppendLiteral :: Addr# -> Utf8Builder ()
unsafeAppendLiteral addr = Utf8Builder $ BB.appendLiteral addr
{-# INLINE unsafeAppendLiteral #-}

-- | Directly append the given byte sequence pointed to by the given address.
-- Be careful that the referenced byte sequence contains valid UTF-8.
--
-- __WARNING__: passing an incorrect length value is likely to cause an access
-- violation or worse.
unsafeAppendLiteralN :: Int -> Addr# -> Utf8Builder ()
unsafeAppendLiteralN !len addr = Utf8Builder $ BB.unsafeAppendLiteralN len addr
{-# INLINE unsafeAppendLiteralN #-}

-- | Directly append the given 'ByteString' to the output buffer.
-- Be careful that the referenced 'ByteString' contains valid UTF-8.
unsafeAppendBS :: ByteString -> Utf8Builder ()
unsafeAppendBS a = Utf8Builder $ BB.appendBS a
{-# INLINE unsafeAppendBS #-}
