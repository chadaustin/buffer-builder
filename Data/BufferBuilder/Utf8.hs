{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BufferBuilder.Utf8 where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.BufferBuilder (BufferBuilder)
import qualified Data.BufferBuilder as BB
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

newtype Utf8Builder a = Utf8Builder { unBuilder :: BufferBuilder a }
    deriving (Functor, Monad)

instance Applicative Utf8Builder where
    pure = return
    left <*> right = do
        f <- left
        a <- right
        return (f a)

runUtf8Builder :: Utf8Builder () -> ByteString
runUtf8Builder = BB.runBufferBuilder . unBuilder

unsafeAppendBS :: ByteString -> Utf8Builder ()
unsafeAppendBS = Utf8Builder . BB.appendBS

appendText :: Text -> Utf8Builder ()
appendText = Utf8Builder . BB.appendBS . encodeUtf8

appendChar8 :: Char -> Utf8Builder ()
appendChar8 = Utf8Builder . BB.appendChar8

appendEscapedJson :: ByteString -> Utf8Builder ()
appendEscapedJson = Utf8Builder . BB.appendEscapedJson
