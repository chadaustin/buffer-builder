{-# LANGUAGE OverloadedStrings, MagicHash, UnboxedTuples, BangPatterns #-}

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
--import GHC.Prim
import GHC.IO
import GHC.ForeignPtr
--import Data.Char (ord)
--import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
--import qualified Data.ByteString.Lazy as BSL

newtype BufferWriter a = BW (BWRep a)
type Buffer# = (# Addr#, Int#, Int#, ForeignPtr Word8 #) -- base, size, capacity, storage
type BWRep a = (# State# RealWorld, Buffer# #) -> (# State# RealWorld, Buffer#, a #)

instance Functor BufferWriter where
    fmap f (BW m) = BW $ \ s ->
        case (m s) of
        (# new_s, n, r #) -> (# new_s, n, f r #)
 
instance Monad BufferWriter where
    return x = BW (\ (# s, n #) -> (# s, n, x #))
    m >> k = m >>= \ _ -> k
 
    (BW m) >>= k
        = BW (\ (# s, n #) ->
        case (m (# s, n #)) of { (# new_s, n2, r #) ->
        case (k r) of { BW k2 ->
        (k2 (# new_s, n2 #)) }}) 

runBufferWriter :: BufferWriter () -> BS.ByteString
runBufferWriter = unsafeDupablePerformIO . (runBufferWriter' 1)

runBufferWriter' :: Int -> BufferWriter () -> IO BS.ByteString
runBufferWriter' !initialCapacity@(I# initialCapacity#) !(BW rep) = do
    fp@(ForeignPtr addr# _) <- mallocForeignPtrBytes initialCapacity
    IO (\s0 -> case rep (# s0, (# addr#, 0#, initialCapacity#, fp #) #) of {
    (# s1, (# _, finalSize, _, finalFP #), _ #) -> (# s1, BS.PS finalFP 0 (I# finalSize) #) })

foreign import ccall unsafe "string.h" memcpy  :: Ptr a -> Ptr a -> Int -> IO ()

grow :: (# State# RealWorld, Buffer# #) -> (# State# RealWorld, Buffer# #)
grow (# s0, (# oldAddr, size, cap, _ #) #) =
    let newCap# = cap *# 2# in
    let (IO iorep) = mallocForeignPtrBytes (I# newCap#) in
    case iorep s0 of {
    (# s1, fp@(ForeignPtr addr# _) #) -> case memcpy (Ptr addr#) (Ptr oldAddr) (I# size) of {
    (IO memcpy') -> case memcpy' s1 of {
    (# s2, _ #) -> (# s2, (# addr#, size, newCap#, fp #) #) }}}

appendByte :: Word8 -> BufferWriter ()
appendByte !(W8# b) = BW (
    \happy@(# _, (# _, size, cap, _ #) #) ->
    case (case I# size >= I# cap of
        True -> grow happy
        False -> happy) of {
    (# s1, (# addr, newSize, newCap, fp #) #) -> case writeWord8OffAddr# addr newSize b s1 of
    s2 -> (# s2, (# addr, newSize +# 1#, newCap, fp #), () #) } )

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
appendBS !bs = BS.foldl' (\m b -> m >> appendByte b) (return ()) bs
