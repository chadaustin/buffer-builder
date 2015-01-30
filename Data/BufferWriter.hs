{-# LANGUAGE OverloadedStrings, MagicHash #-}

module Data.BufferWriter
    ( BufferWriter
    , runBufferWriter
    , appendByte
    , appendChar8
    , appendBS
    ) where

import Data.Monoid ((<>))
import Data.Char (ord)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Control.Monad.State.Lazy as State

type BufferWriter = State.State BS.ByteString ()

runBufferWriter :: BufferWriter -> BS.ByteString
runBufferWriter st = snd $ State.runState st ""

appendByte :: Word8 -> BufferWriter
appendByte b = State.modify' (\s -> BS.snoc s b)

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

appendChar8 :: Char -> BufferWriter
appendChar8 = appendByte . c2w

appendBS :: BS.ByteString -> BufferWriter
appendBS bs = State.modify' (<> bs)
