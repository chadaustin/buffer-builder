{-# LANGUAGE OverloadedStrings, MagicHash #-}

module Data.BufferWriter
    ( BufferWriter
    , runBufferWriter
    , appendByte
    , appendChar8
    , appendBS
    ) where

import Data.Monoid ((<>), mempty)
import Data.Char (ord)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Control.Monad.State.Lazy as State

type BufferWriter = State.State BSB.Builder ()

runBufferWriter :: BufferWriter -> BS.ByteString
runBufferWriter st = BSL.toStrict $ BSB.toLazyByteString $ snd $ State.runState st mempty

appendByte :: Word8 -> BufferWriter
appendByte b = State.modify' (<> BSB.word8 b)

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

appendChar8 :: Char -> BufferWriter
appendChar8 = appendByte . c2w

appendBS :: BS.ByteString -> BufferWriter
appendBS bs = State.modify' (<> BSB.byteString bs)
