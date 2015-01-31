{-# LANGUAGE OverloadedStrings, MagicHash, UnboxedTuples, BangPatterns, GeneralizedNewtypeDeriving #-}

module Data.BufferWriter
    ( BufferWriter
    , runBufferWriter
    , appendByte
    , appendChar8
    , appendBS
    ) where

import GHC.Base
import GHC.Word
import GHC.Ptr
import GHC.IO
import GHC.ForeignPtr
import Foreign.ForeignPtr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Control.Monad.Reader

data BWHandle'
type BWHandle = Ptr BWHandle'

foreign import ccall unsafe "bw_new" bw_new :: Int -> IO BWHandle
foreign import ccall unsafe "&bw_free" bw_free :: FunPtr (BWHandle -> IO ())
foreign import ccall unsafe "bw_append_byte" bw_append_byte :: BWHandle -> Word8 -> IO ()
foreign import ccall unsafe "bw_append_bs" bw_append_bs :: BWHandle -> Int -> (Ptr Word8) -> IO ()
foreign import ccall unsafe "bw_get_size" bw_get_size :: BWHandle -> IO Int
foreign import ccall unsafe "bw_get_address" bw_get_address :: BWHandle -> IO (Ptr Word8)

newtype BufferWriter a = BW (ReaderT BWHandle IO a)
    deriving (Functor, Monad, MonadReader BWHandle)

unBW :: BufferWriter a -> ReaderT BWHandle IO a
unBW (BW bw) = bw

inBW :: IO a -> BufferWriter a
inBW = BW . lift

runBufferWriter :: BufferWriter () -> BS.ByteString
runBufferWriter = unsafeDupablePerformIO . (runBufferWriterIO 16)

runBufferWriterIO :: Int -> BufferWriter () -> IO BS.ByteString
runBufferWriterIO !initialCapacity !inner = do
    initial' <- bw_new initialCapacity
    initial <- newForeignPtr bw_free initial'
    _ <- runReaderT (unBW inner) initial'
    size <- bw_get_size initial'   
    src <- bw_get_address initial'
    rv <- BS.create size $ \dst ->
        BS.memcpy dst src size
    touchForeignPtr initial
    return rv

appendByte :: Word8 -> BufferWriter ()
appendByte b = do
    h <- ask
    inBW $ bw_append_byte h b

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

appendChar8 :: Char -> BufferWriter ()
appendChar8 = appendByte . c2w

-- TODO: optimize
appendBS :: BS.ByteString -> BufferWriter ()
appendBS !(BS.PS (ForeignPtr addr _) offset len) = do
    h <- ask
    inBW $ bw_append_bs h len (plusPtr (Ptr addr) offset)
