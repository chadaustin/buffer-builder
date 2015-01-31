{-# LANGUAGE OverloadedStrings, MagicHash, UnboxedTuples, BangPatterns #-}

module Data.BufferWriter
    ( BufferWriter
    , runBufferWriter
    , appendByte
    , appendChar8
    , appendBS
    ) where

import GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Ptr
import GHC.IO
import GHC.ForeignPtr
import Foreign.ForeignPtr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Control.Monad.State.Strict

data BWHandle'
type BWHandle = Ptr BWHandle'

foreign import ccall unsafe "bw_new" bw_new :: Int -> IO BWHandle
foreign import ccall unsafe "&bw_free" bw_free :: FunPtr (BWHandle -> IO ())
foreign import ccall unsafe "bw_append_byte" bw_append_byte :: BWHandle -> Word8 -> IO ()
foreign import ccall unsafe "bw_append_bs" bw_append_bs :: BWHandle -> Int -> (Ptr Word8) -> IO ()
foreign import ccall unsafe "bw_get_size" bw_get_size :: BWHandle -> IO Int
foreign import ccall unsafe "bw_get_address" bw_get_address :: BWHandle -> IO (Ptr Word8)

type BufferWriter a = (StateT (ForeignPtr BWHandle') IO a)

{-
instance Functor BufferWriter where
    fmap f (BW m) = BW $ \ s ->
        case (m s) of
        (# new_s, n, r #) -> (# new_s, n, f r #)
 
instance Monad BufferWriter where
    {-# INLINE return #-}
    {-# INLINE (>>) #-}
    {-# INLINE (>>=) #-}

    return x = BW (\ (# s, n #) -> (# s, n, x #))
    m >> k = m >>= \ _ -> k
 
    (BW m) >>= k
        = BW (\ (# s, n #) ->
        case (m (# s, n #)) of { (# new_s, n2, r #) ->
        case (k r) of { BW k2 ->
        (k2 (# new_s, n2 #)) }}) 
-}

runBufferWriter :: BufferWriter () -> BS.ByteString
runBufferWriter = unsafeDupablePerformIO . (runBufferWriterIO 16)

runBufferWriterIO :: Int -> BufferWriter () -> IO BS.ByteString
runBufferWriterIO !initialCapacity !inner = do
    initial' <- bw_new initialCapacity
    initial <- newForeignPtr bw_free initial'
    (_, final) <- runStateT inner initial
    size <- bw_get_size initial'   
    src <- bw_get_address initial'
    rv <- BS.create size $ \dst ->
        BS.memcpy dst src size
    touchForeignPtr initial
    return rv
    --fp@(ForeignPtr addr# _) <- mallocForeignPtrBytes initialCapacity
    --IO (\s0 -> case rep (# s0, (# addr#, 0#, initialCapacity#, fp #) #) of {
    --(# s1, (# _, finalSize, _, finalFP #), _ #) -> (# s1, BS.PS finalFP 0 (I# finalSize) #) })

{-
grow :: (# State# RealWorld, Buffer# #) -> (# State# RealWorld, Buffer# #)
grow (# s0, (# oldAddr, size, cap, _ #) #) =
    let newCap# = cap *# 2# in
    let (IO iorep) = mallocForeignPtrBytes (I# newCap#) in
    case iorep s0 of {
    (# s1, fp@(ForeignPtr addr# _) #) -> case memcpy (Ptr addr#) (Ptr oldAddr) (I# size) of {
    (IO memcpy') -> case memcpy' s1 of {
    (# s2, _ #) -> (# s2, (# addr#, size, newCap#, fp #) #) }}}
{-# NOINLINE grow #-}
-}

appendByte :: Word8 -> BufferWriter ()
appendByte b = do
    h <- get
    lift $ withForeignPtr h $ \g -> bw_append_byte g b
{-
    \happy@(# _, (# _, size, cap, _ #) #) ->
    case (case I# size >= I# cap of
        True -> grow happy
        False -> happy) of {
    (# s1, (# addr, newSize, newCap, fp #) #) -> case writeWord8OffAddr# addr newSize b s1 of
    s2 -> (# s2, (# addr, newSize +# 1#, newCap, fp #), () #) } )
-}

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
    h <- get
    lift $ withForeignPtr h $ \g -> bw_append_bs g len (plusPtr (Ptr addr) offset)
