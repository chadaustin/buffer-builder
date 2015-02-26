{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns, RecordWildCards, DeriveDataTypeable #-}

{-|
A library for efficiently building up a buffer of data.  When given data
known to be strict, use of BufferBuilder compiles directly into a series
of efficient C function calls.
-}
module Data.BufferBuilder (

    -- * The BufferBuilder Monad
      BufferBuilder
    , runBufferBuilder

    -- * Optional configuration
    , Options(..)
    , runBufferBuilderWithOptions

    -- * Appending bytes and byte strings
    , appendByte
    , appendChar8
    , appendBS
    , appendLBS
    , appendLiteral
    , unsafeAppendLiteralN

    -- * Appending bytes and byte strings, truncated to 7 bits
    , appendByte7
    , appendChar7
    , appendBS7
    , appendLiteral7
    , unsafeAppendLiteralN7

    -- * UTF-8 encoding
    , appendCharUtf8
    , appendStringUtf8

    -- * Printing numbers
    , appendDecimalSignedInt
    , appendDecimalDouble

    -- * JSON escaping
    , appendEscapedJson
    , appendEscapedJsonLiteral
    , appendEscapedJsonText

    -- * URL percent-encoding
    , appendUrlEncoded
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
import Control.Applicative (Applicative(..), pure)
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Data.Typeable (Typeable)

import Data.Text () -- Show
import Data.Text.Internal (Text (..))
import Data.Text.Array (Array (..))

data Handle'
type Handle = Ptr Handle'

foreign import ccall unsafe "strlen" c_strlen :: Ptr Word8 -> IO Int
foreign import ccall unsafe "bw_new" bw_new :: Int -> IO Handle
foreign import ccall unsafe "&bw_free" bw_free :: FunPtr (Handle -> IO ())
foreign import ccall unsafe "bw_trim" bw_trim :: Handle -> IO ()
foreign import ccall unsafe "bw_get_size" bw_get_size :: Handle -> IO Int
foreign import ccall unsafe "bw_release_address" bw_release_address :: Handle -> IO (Ptr Word8)

foreign import ccall unsafe "bw_append_byte" bw_append_byte :: Handle -> Word8 -> IO ()
foreign import ccall unsafe "bw_append_char_utf8" bw_append_char_utf8 :: Handle -> Char -> IO ()
foreign import ccall unsafe "bw_append_bs" bw_append_bs :: Handle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_bsz" bw_append_bsz :: Handle -> Ptr Word8 -> IO ()

foreign import ccall unsafe "bw_append_byte7" bw_append_byte7 :: Handle -> Word8 -> IO ()
foreign import ccall unsafe "bw_append_bs7" bw_append_bs7 :: Handle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_bsz7" bw_append_bsz7 :: Handle -> Ptr Word8 -> IO ()

foreign import ccall unsafe "bw_append_decimal_signed_int" bw_append_decimal_signed_int :: Handle -> Int -> IO ()
foreign import ccall unsafe "bw_append_decimal_double" bw_append_decimal_double :: Handle -> Double -> IO ()

foreign import ccall unsafe "bw_append_json_escaped" bw_append_json_escaped :: Handle -> Int -> Ptr Word8 -> IO ()
foreign import ccall unsafe "bw_append_json_escaped_utf16" bw_append_json_escaped_utf16 :: Handle -> Int -> Ptr Word16 -> IO ()

foreign import ccall unsafe "bw_append_url_encoded" bw_append_url_encoded :: Handle -> Int -> Ptr Word8 -> IO ()

-- | BufferBuilder is the type of a monadic action that appends to an implicit,
-- growable buffer.  Use 'runBufferBuilder' to extract the resulting
-- buffer as a 'BS.ByteString'.  
newtype BufferBuilder a = BB (Handle -> IO a)
    --deriving (Applicative, Monad, MonadReader Handle)

unBB :: BufferBuilder a -> (Handle -> IO a)
unBB (BB a) = a

instance Functor BufferBuilder where
    {-# INLINE fmap #-}
    fmap f (BB a) = BB $ \h -> fmap f (a h)

instance Applicative BufferBuilder where
    {-# INLINE pure #-}
    pure = BB . const . pure

    {-# INLINE (<*>) #-}
    (BB f) <*> (BB a) = BB $ \h -> (f h) <*> (a h)

instance Monad BufferBuilder where
    {-# INLINE return #-}
    return = BB . const . return

    {-# INLINE (>>=) #-}
    (BB lhs) >>= next = BB $ \h -> do
        a <- lhs h
        unBB (next a) h

withHandle :: (Handle -> IO ()) -> BufferBuilder ()
withHandle = BB
{-# INLINE withHandle #-}

data BufferOutOfMemoryError = BufferOutOfMemoryError
    deriving (Show, Typeable)
instance Exception BufferOutOfMemoryError

data Options = Options
    { initialCapacity :: !Int
    , trimFinalBuffer :: !Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { initialCapacity = 128 -- some quantitative data would be great
    , trimFinalBuffer = False
    }

-- | Run a sequence of 'BufferBuilder' actions and extract the resulting
-- buffer as a 'BS.ByteString'.
runBufferBuilder :: BufferBuilder () -> BS.ByteString
runBufferBuilder = runBufferBuilderWithOptions defaultOptions

runBufferBuilderWithOptions :: Options -> BufferBuilder () -> BS.ByteString
runBufferBuilderWithOptions options = unsafeDupablePerformIO . runBufferBuilderIO options

runBufferBuilderIO :: Options-> BufferBuilder () -> IO BS.ByteString
runBufferBuilderIO !Options{..} !(BB bw) = do
    handle <- bw_new initialCapacity
    when (handle == nullPtr) $ do
        throw BufferOutOfMemoryError
    
    handleFP <- newForeignPtr bw_free handle
    () <- bw handle

    when trimFinalBuffer $ do
        bw_trim handle

    -- FFI doesn't support returning multiple arguments, so we need
    -- two calls: one for the size and the other to release the
    -- pointer.
    size <- bw_get_size handle
    src <- bw_release_address handle

    when (src == nullPtr) $ do
        throw BufferOutOfMemoryError

    borrowed <- newForeignPtr finalizerFree src
    let bs = BS.fromForeignPtr borrowed 0 size
    touchForeignPtr handleFP
    return bs

-- | Append a single byte to the output buffer.  To append multiple bytes in sequence and
-- avoid redundant bounds checks, consider using 'appendBS', 'appendLiteral', or 'unsafeAppendLiteralN'.
appendByte :: Word8 -> BufferBuilder ()
appendByte b = withHandle $ \h -> bw_append_byte h b
{-# INLINE appendByte #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | Appends a character to the buffer, truncating it to the bottom 8 bits.
appendChar8 :: Char -> BufferBuilder ()
appendChar8 = appendByte . c2w
{-# INLINE appendChar8 #-}

-- | Appends a 'BS.ByteString' to the buffer.  When appending constant, hardcoded strings, to
-- avoid a CAF and the costs of its associated tag check and indirect jump, use
-- 'appendLiteral' or 'unsafeAppendLiteralN' instead.
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
-- 'unsafeAppendLiteralN' is faster, as 'unsafeAppendLiteralN' avoids a strlen
-- operation which has nontrivial cost in some benchmarks.
appendLiteral :: Addr# -> BufferBuilder ()
appendLiteral addr =
    withHandle $ \h ->
        bw_append_bsz h (Ptr addr)
{-# INLINE appendLiteral #-}

-- | Appends a MagicHash string literal with a known length.  Use this when the
-- string literal's length is known.  For example:
--
-- > unsafeAppendLiteralN 4 "true"#
--
-- Per byte, this is the fastest append function.  It amounts to a C function call
-- with two constant arguments.  The C function checks to see if it needs to grow
-- the buffer and then it simply calls memcpy.
-- 
-- __WARNING__: passing an incorrect length value is likely to cause an access
-- violation or worse.
unsafeAppendLiteralN :: Int -> Addr# -> BufferBuilder ()
unsafeAppendLiteralN len addr =
    withHandle $ \h ->
        bw_append_bs h len (Ptr addr)
{-# INLINE unsafeAppendLiteralN #-}


-- 7-bit truncation

appendByte7 :: Word8 -> BufferBuilder ()
appendByte7 b = withHandle $ \h -> bw_append_byte7 h b
{-# INLINE appendByte7 #-}

appendChar7 :: Char -> BufferBuilder ()
appendChar7 = appendByte7 . c2w
{-# INLINE appendChar7 #-}

appendBS7 :: BS.ByteString -> BufferBuilder ()
appendBS7 !(BS.PS fp offset len) =
    withHandle $ \h ->
        withForeignPtr fp $ \addr ->
            bw_append_bs7 h len (plusPtr addr offset)
{-# INLINE appendBS7 #-}

appendLiteral7 :: Addr# -> BufferBuilder ()
appendLiteral7 addr =
    withHandle $ \h ->
        bw_append_bsz7 h (Ptr addr)
{-# INLINE appendLiteral7 #-}

unsafeAppendLiteralN7 :: Int -> Addr# -> BufferBuilder ()
unsafeAppendLiteralN7 len addr =
    withHandle $ \h ->
        bw_append_bs7 h len (Ptr addr)
{-# INLINE unsafeAppendLiteralN7 #-}

-- Encoding UTF-8

-- | Appends a UTF-8-encoded 'Char' to the buffer.
appendCharUtf8 :: Char -> BufferBuilder ()
appendCharUtf8 c = withHandle $ \h -> bw_append_char_utf8 h c
{-# INLINE appendCharUtf8 #-}

-- | Appends a UTF-8-encoded 'String' to the buffer.  The best way to improve performance here
-- is to use 'BS.ByteString' or 'Text' instead of 'String'.
appendStringUtf8 :: String -> BufferBuilder ()
appendStringUtf8 = mapM_ appendCharUtf8
{-# INLINABLE appendStringUtf8 #-}


-- Printing Numbers

-- | Appends a decimal integer, just like calling printf("%d", ...)
appendDecimalSignedInt :: Int -> BufferBuilder ()
appendDecimalSignedInt i =
    withHandle $ \h ->
        bw_append_decimal_signed_int h i
{-# INLINE appendDecimalSignedInt #-}

-- | Appends a decimal double, just like calling printf("%f", ...)
appendDecimalDouble :: Double -> BufferBuilder ()
appendDecimalDouble d =
    withHandle $ \h ->
        bw_append_decimal_double h d
{-# INLINE appendDecimalDouble #-}


-- Encoding JSON

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
{-# INLINE appendEscapedJsonLiteral #-}

appendEscapedJsonText :: Text -> BufferBuilder ()
appendEscapedJsonText !(Text arr ofs len) =
    let byteArray = aBA arr
    in withHandle $ \h ->
        bw_append_json_escaped_utf16 h len (Ptr (byteArrayContents# byteArray) `plusPtr` ofs)
{-# INLINE appendEscapedJsonText #-}

appendUrlEncoded :: BS.ByteString -> BufferBuilder ()
appendUrlEncoded !(BS.PS (ForeignPtr addr _) offset len) =
    withHandle $ \h ->
        bw_append_url_encoded h len (plusPtr (Ptr addr) offset)
{-# INLINE appendUrlEncoded #-}
