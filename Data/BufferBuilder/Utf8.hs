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

    -- * Printing numbers
    , appendDecimalSignedInt
    , appendDecimalDouble

    -- * Escaped JSON
    , appendEscapedJson
    , appendEscapedJsonLiteral
    , appendEscapedJsonText

    -- * Unsafe append operations
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

-- TODO: optimize with custom encoder
appendText :: Text -> Utf8Builder ()
appendText a = Utf8Builder $ BB.appendBS $ encodeUtf8 a
{-# INLINE appendText #-}

appendString :: String -> Utf8Builder ()
appendString s = mapM_ appendChar s
{-# INLINABLE appendString #-}

appendChar :: Char -> Utf8Builder ()
appendChar c = Utf8Builder $ BB.appendCharUtf8 c
{-# INLINE appendChar #-}


-- ASCII-7

appendByte7 :: Word8 -> Utf8Builder ()
appendByte7 = Utf8Builder . BB.appendByte7
{-# INLINE appendByte7 #-}

appendChar7 :: Char -> Utf8Builder ()
appendChar7 = Utf8Builder . BB.appendChar7
{-# INLINE appendChar7 #-}

appendBS7 :: ByteString -> Utf8Builder ()
appendBS7 = Utf8Builder . BB.appendBS7
{-# INLINE appendBS7 #-}

appendLiteral7 :: Addr# -> Utf8Builder ()
appendLiteral7 addr = Utf8Builder $ BB.appendLiteral7 addr
{-# INLINE appendLiteral7 #-}


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

unsafeAppendByte :: Word8 -> Utf8Builder ()
unsafeAppendByte = Utf8Builder . BB.appendByte
{-# INLINE unsafeAppendByte #-}

unsafeAppendChar8 :: Char -> Utf8Builder ()
unsafeAppendChar8 = Utf8Builder . BB.appendChar8
{-# INLINE unsafeAppendChar8 #-}

unsafeAppendLiteral :: Addr# -> Utf8Builder ()
unsafeAppendLiteral addr = Utf8Builder $ BB.appendLiteral addr
{-# INLINE unsafeAppendLiteral #-}

unsafeAppendLiteralN :: Int -> Addr# -> Utf8Builder ()
unsafeAppendLiteralN !len addr = Utf8Builder $ BB.unsafeAppendLiteralN len addr
{-# INLINE unsafeAppendLiteralN #-}

unsafeAppendBS :: ByteString -> Utf8Builder ()
unsafeAppendBS a = Utf8Builder $ BB.appendBS a
{-# INLINE unsafeAppendBS #-}
