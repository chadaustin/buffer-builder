{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.BufferBuilder.Utf8
       ( runUtf8Builder
       , Utf8Builder
       , appendText
       , appendString
       , appendChar
       , appendChar8
       , unsafeAppendLiteral
       , unsafeAppendLiteralN
       , unsafeAppendBS
       , appendDecimalDouble
       , appendDecimalSignedInt
       , appendEscapedJsonLiteral
       , appendEscapedJsonText
       , appendEscapedJson
       ) where

import           GHC.Base
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.BufferBuilder (BufferBuilder)
import qualified Data.BufferBuilder as BB
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

newtype Utf8Builder a = Utf8Builder { unBuilder :: BufferBuilder a }
    deriving (Functor, Applicative, Monad)

runUtf8Builder :: Utf8Builder () -> ByteString
runUtf8Builder a = BB.runBufferBuilder $ unBuilder a
{-# INLINE runUtf8Builder #-}

unsafeAppendLiteral :: Addr# -> Utf8Builder ()
unsafeAppendLiteral addr = Utf8Builder $ BB.appendLiteral addr
{-# INLINE unsafeAppendLiteral #-}

unsafeAppendLiteralN :: Int -> Addr# -> Utf8Builder ()
unsafeAppendLiteralN !len addr = Utf8Builder $ BB.appendLiteralN len addr
{-# INLINE unsafeAppendLiteralN #-}

unsafeAppendBS :: ByteString -> Utf8Builder ()
unsafeAppendBS a = Utf8Builder $ BB.appendBS a
{-# INLINE unsafeAppendBS #-}

appendString :: String -> Utf8Builder ()
appendString s = mapM_ appendChar s

appendChar :: Char -> Utf8Builder ()
appendChar c = Utf8Builder $ BB.appendCharUtf8 c
{-# INLINE appendChar #-}

appendText :: Text -> Utf8Builder ()
appendText a = Utf8Builder $ BB.appendBS $ encodeUtf8 a
{-# INLINE appendText #-}

appendChar8 :: Char -> Utf8Builder ()
appendChar8 a = Utf8Builder $ BB.appendChar8 a
{-# INLINE appendChar8 #-}

appendEscapedJsonLiteral :: Addr# -> Utf8Builder ()
appendEscapedJsonLiteral addr = Utf8Builder $ BB.appendEscapedJsonLiteral addr

appendEscapedJson :: ByteString -> Utf8Builder ()
appendEscapedJson a = Utf8Builder $ BB.appendEscapedJson a
{-# INLINE appendEscapedJson #-}

appendEscapedJsonText :: Text -> Utf8Builder ()
appendEscapedJsonText txt = Utf8Builder $ BB.appendEscapedJsonText txt
{-# INLINE appendEscapedJsonText #-}

appendDecimalSignedInt :: Int -> Utf8Builder ()
appendDecimalSignedInt a = Utf8Builder $ BB.appendDecimalSignedInt a
{-# INLINE appendDecimalSignedInt #-}

appendDecimalDouble :: Double -> Utf8Builder ()
appendDecimalDouble d = Utf8Builder $ BB.appendDecimalDouble d
{-# INLINE appendDecimalDouble #-}
