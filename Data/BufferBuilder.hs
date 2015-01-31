{-# LANGUAGE OverloadedStrings, MagicHash, UnboxedTuples, BangPatterns, GeneralizedNewtypeDeriving #-}

module Data.BufferBuilder
    ( BufferBuilder
    , runBufferBuilder
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
import Foreign.Marshal.Alloc
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
foreign import ccall unsafe "bw_trim_and_release_address" bw_trim_and_release_address :: BWHandle -> IO (Ptr Word8)

newtype BufferBuilder a = BB (ReaderT BWHandle IO a)
    deriving (Functor, Monad, MonadReader BWHandle)

inBW :: IO a -> BufferBuilder a
inBW = BB . lift

initialCapacity :: Int
initialCapacity = 48
-- why 48? it's only 6 64-bit words...  yet many small strings should fit.
-- some quantitative analysis would be good.
-- an option to set the initial capacity would be better. :)

runBufferBuilder :: BufferBuilder () -> BS.ByteString
runBufferBuilder = unsafeDupablePerformIO . runBufferBuilderIO initialCapacity

runBufferBuilderIO :: Int -> BufferBuilder () -> IO BS.ByteString
runBufferBuilderIO !capacity !(BB bw) = do
    handle <- bw_new capacity
    handleFP <- newForeignPtr bw_free handle
    () <- runReaderT bw handle
    size <- bw_get_size handle
    src <- bw_trim_and_release_address handle

    borrowed <- newForeignPtr finalizerFree src
    let bs = BS.fromForeignPtr borrowed 0 size
    touchForeignPtr handleFP
    return bs

appendByte :: Word8 -> BufferBuilder ()
appendByte b = do
    h <- ask
    inBW $ bw_append_byte h b
{-# INLINE appendByte #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

appendChar8 :: Char -> BufferBuilder ()
appendChar8 = appendByte . c2w

appendBS :: BS.ByteString -> BufferBuilder ()
appendBS !(BS.PS (ForeignPtr addr _) offset len) = do
    h <- ask
    inBW $ bw_append_bs h len (plusPtr (Ptr addr) offset)
{-# INLINE appendBS #-}

