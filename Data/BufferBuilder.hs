{-# LANGUAGE OverloadedStrings, MagicHash, UnboxedTuples, BangPatterns, GeneralizedNewtypeDeriving #-}

module Data.BufferBuilder
    ( BufferBuilder
    , runBufferBuilder
    , appendByte
    , appendChar8
    , appendBS
    , appendLiteral
    , appendLiteralN
    , appendEscapedJson
    , appendEscapedJsonLiteral
    , appendEscapedJsonText
    , appendDecimalSignedInt
    , appendDecimalDouble
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
import Control.Applicative (Applicative)

import Data.Text () -- Show
import Data.Text.Internal (Text (..))
import Data.Text.Array (Array (..))

data BWHandle'
type BWHandle = Ptr BWHandle'

foreign import ccall unsafe "strlen" c_strlen :: Ptr Word8 -> IO Int
foreign import ccall unsafe "bw_new" bw_new :: Int -> IO BWHandle
foreign import ccall unsafe "&bw_free" bw_free :: FunPtr (BWHandle -> IO ())
foreign import ccall unsafe "bw_append_byte" bw_append_byte :: BWHandle -> Word8 -> IO ()
foreign import ccall unsafe "bw_append_bs" bw_append_bs :: BWHandle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_bsz" bw_append_bsz :: BWHandle -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_get_size" bw_get_size :: BWHandle -> IO Int
foreign import ccall unsafe "bw_trim_and_release_address" bw_trim_and_release_address :: BWHandle -> IO (Ptr Word8)
foreign import ccall unsafe "bw_append_json_escaped" bw_append_json_escaped :: BWHandle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_json_escaped_utf16" bw_append_json_escaped_utf16 :: BWHandle -> Int -> Ptr Word16 -> IO ()
foreign import ccall unsafe "bw_append_decimal_signed_int" bw_append_decimal_signed_int :: BWHandle -> Int -> IO ()
foreign import ccall unsafe "bw_append_decimal_double" bw_append_decimal_double :: BWHandle -> Double -> IO ()

-- | BufferBuilder sequences actions that append to an implicit,
-- growable buffer.  Use 'runBufferBuilder' to extract the resulting
-- buffer as a 'BS.ByteString'
newtype BufferBuilder a = BB (ReaderT BWHandle IO a)
    deriving (Functor, Applicative, Monad, MonadReader BWHandle)

inBW :: IO a -> BufferBuilder a
inBW = BB . lift

withHandle :: (BWHandle -> IO ()) -> BufferBuilder ()
withHandle action = do
    h <- ask
    inBW $ action h
{-# INLINE withHandle #-}

initialCapacity :: Int
initialCapacity = 48
-- why 48? it's only 6 64-bit words...  yet many small strings should fit.
-- some quantitative analysis would be good.
-- an option to set the initial capacity would be better. :)

-- | Runs a BufferBuilder and extracts its resulting contents as a 'BS.ByteString'
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


appendByte :: Word8 -- ^ byte to append to the buffer.
           -> BufferBuilder ()
appendByte b = withHandle $ \h -> bw_append_byte h b
{-# INLINE appendByte #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | Appends a character to the buffer, truncating it to the bottom 8 bits.
appendChar8 :: Char -- ^ character to append to the buffer
            -> BufferBuilder ()
appendChar8 = appendByte . c2w
{-# INLINE appendChar8 #-}

-- | Appends a ByteString to the buffer.
appendBS :: BS.ByteString -- ^ 'BS.ByteString' to append
         -> BufferBuilder ()
appendBS !(BS.PS (ForeignPtr addr _) offset len) =
    withHandle $ \h ->
        bw_append_bs h len (plusPtr (Ptr addr) offset)
{-# INLINE appendBS #-}

appendLiteral :: Addr# -> BufferBuilder ()
appendLiteral addr =
    withHandle $ \h ->
        bw_append_bsz h (Ptr addr)
{-# INLINE appendLiteral #-}

appendLiteralN :: Int -> Addr# -> BufferBuilder ()
appendLiteralN len addr = 
    withHandle $ \h ->
        bw_append_bs h len (Ptr addr)
{-# INLINE appendLiteralN #-}

appendEscapedJson :: BS.ByteString -> BufferBuilder ()
appendEscapedJson !(BS.PS (ForeignPtr addr _) offset len) =
    withHandle $ \h ->
        bw_append_json_escaped h len (plusPtr (Ptr addr) offset)
{-# INLINE appendEscapedJson #-}

appendEscapedJsonLiteral :: Addr# -> BufferBuilder ()
appendEscapedJsonLiteral addr =
    withHandle $ \h -> do
        len <- c_strlen (Ptr addr)
        bw_append_json_escaped h len (Ptr addr)

appendEscapedJsonText :: Text -> BufferBuilder ()
appendEscapedJsonText !(Text !(Array byteArray) ofs len) =
    withHandle $ \h ->
        bw_append_json_escaped_utf16 h len (Ptr (byteArrayContents# byteArray) `plusPtr` ofs)
{-# INLINE appendEscapedJsonText #-}

appendDecimalSignedInt :: Int -> BufferBuilder ()
appendDecimalSignedInt i =
    withHandle $ \h ->
        bw_append_decimal_signed_int h i
{-# INLINE appendDecimalSignedInt #-}

appendDecimalDouble :: Double -> BufferBuilder ()
appendDecimalDouble d =
    withHandle $ \h ->
        bw_append_decimal_double h d
{-# INLINE appendDecimalDouble #-}
