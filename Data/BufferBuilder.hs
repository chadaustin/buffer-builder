{-# LANGUAGE OverloadedStrings, MagicHash, UnboxedTuples, BangPatterns, GeneralizedNewtypeDeriving #-}

{-|
A library for efficiently building up a buffer of data.  When given known strict data, the
implementation compiles directly into a series of efficient C function calls.
-}
module Data.BufferBuilder (
    -- * The BufferBuilder Monad
      BufferBuilder
    , runBufferBuilder
    -- * Appending bytes and byte strings
    , appendByte
    , appendChar8
    , appendBS
    , appendLBS
    , appendLiteral
    , appendLiteralN
    -- * UTF-8 encoding
    , appendCharUtf8
    , appendStringUtf8
    -- * JSON escaping
    , appendEscapedJson
    , appendEscapedJsonLiteral
    , appendEscapedJsonText
    -- * Printing numbers
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
import qualified Data.ByteString.Lazy as BSL
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
foreign import ccall unsafe "bw_get_size" bw_get_size :: BWHandle -> IO Int
foreign import ccall unsafe "bw_trim_and_release_address" bw_trim_and_release_address :: BWHandle -> IO (Ptr Word8)
foreign import ccall unsafe "bw_append_byte" bw_append_byte :: BWHandle -> Word8 -> IO ()
foreign import ccall unsafe "bw_append_char_utf8" bw_append_char_utf8 :: BWHandle -> Char -> IO ()
foreign import ccall unsafe "bw_append_bs" bw_append_bs :: BWHandle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_bsz" bw_append_bsz :: BWHandle -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_json_escaped" bw_append_json_escaped :: BWHandle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_json_escaped_utf16" bw_append_json_escaped_utf16 :: BWHandle -> Int -> Ptr Word16 -> IO ()
foreign import ccall unsafe "bw_append_decimal_signed_int" bw_append_decimal_signed_int :: BWHandle -> Int -> IO ()
foreign import ccall unsafe "bw_append_decimal_double" bw_append_decimal_double :: BWHandle -> Double -> IO ()

-- | BufferBuilder is the type of a monadic action that appends to an implicit,
-- growable buffer.  Use 'runBufferBuilder' to extract the resulting
-- buffer as a 'BS.ByteString'.  
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

-- | Runs a sequence of 'BufferBuilder' actions, extracting the resulting
-- contents as a 'BS.ByteString'.
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

-- | Append a single byte to the output buffer.  To append multiple bytes in sequence and
-- avoid redundant bounds checks, consider using 'appendBS', 'appendLiteral', or 'appendLiteralN'.
appendByte :: Word8 -> BufferBuilder ()
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

-- | Appends a 'BS.ByteString' to the buffer.  When appending constant, hardcoded strings, to
-- avoid a CAF and the costs of its associated tag check and indirect jump, use
-- 'appendLiteral' or 'appendLiteralN' instead.
appendBS :: BS.ByteString -> BufferBuilder ()
appendBS !(BS.PS fp offset len) =
    withHandle $ \h ->
        withForeignPtr fp $ \addr ->
            bw_append_bs h len (plusPtr addr offset)
{-# INLINE appendBS #-}

-- | Appends a lazy 'BSL.ByteString' to the buffer.  This function operates by traversing
-- the lazy 'BSL.ByteString' chunks, appending each in turn.
appendLBS :: BSL.ByteString -> BufferBuilder ()
appendLBS lbs = mapM_ appendBS $ BSL.toChunks lbs
{-# INLINABLE appendLBS #-}

-- | Appends a zero-terminated MagicHash string literal.  Use this function instead of
-- 'appendBS' for string constants.  For example:
--
-- > appendLiteral "true"#
--
-- If the length of the string literal is known, calling
-- 'appendLiteralN' is faster, as 'appendLiteralN' avoids a strlen
-- operation which has nontrivial cost in some benchmarks.
appendLiteral :: Addr# -> BufferBuilder ()
appendLiteral addr =
    withHandle $ \h ->
        bw_append_bsz h (Ptr addr)
{-# INLINE appendLiteral #-}

-- | Appends a MagicHash string literal with a known length.  Use this when the
-- string literal's length is known.  For example:
--
-- > appendLiteralN 4 "true"#
--
-- Per byte, this is the fastest append function.  It amounts to a C function call
-- with two constant arguments.  The C function checks to see if it needs to grow
-- the buffer and then it simply calls memcpy.
-- 
-- __WARNING__: passing an incorrect length value is likely to cause an access
-- violation or worse.
appendLiteralN :: Int -> Addr# -> BufferBuilder ()
appendLiteralN len addr = 
    withHandle $ \h ->
        bw_append_bs h len (Ptr addr)
{-# INLINE appendLiteralN #-}


-- UTF-8 Functions


-- | Appends a UTF-8-encoded 'Char' to the buffer.
appendCharUtf8 :: Char -> BufferBuilder ()
appendCharUtf8 c = withHandle $ \h -> bw_append_char_utf8 h c
{-# INLINE appendCharUtf8 #-}

-- | Appends a UTF-8-encoded 'String' to the buffer.  The best way to improve performance here
-- is to use 'BS.ByteString' or 'Text' instead of 'String'.
appendStringUtf8 :: String -> BufferBuilder ()
appendStringUtf8 = mapM_ appendCharUtf8
{-# INLINABLE appendStringUtf8 #-}

-- JSON Functions


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


-- Number Functions


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
