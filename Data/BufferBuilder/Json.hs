{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BufferBuilder.Json where

import           Data.BufferBuilder.Utf8 (Utf8Builder)
import qualified Data.BufferBuilder.Utf8 as BB
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as Builder
import           Data.Foldable (for_)
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text.Encoding as DTE

newtype JsonBuilder   = JsonBuilder { unJsonBuilder :: Utf8Builder () }
newtype ObjectBuilder = ObjectBuilder { unObjectBuilder :: Utf8Builder () }
newtype ArrayBuilder  = ArrayBuilder { unArrayBuilder :: Utf8Builder () }

encodeJson :: ToJson a => a -> ByteString
encodeJson = BB.runUtf8Builder . unJsonBuilder . appendJson

class ToJson a where
    appendJson :: a -> JsonBuilder

emptyObject :: JsonBuilder
emptyObject = JsonBuilder $ do
    BB.appendChar8 '{'
    BB.appendChar8 '}'

array :: ToJson a => [a] -> JsonBuilder
array collection = JsonBuilder $ do
    BB.appendChar8 '['
    case collection of
        [] -> return ()
        [x] -> unJsonBuilder $ appendJson x
        x:xs -> do
            unJsonBuilder $ appendJson x
            for_ xs $ \el -> do
                BB.appendChar8 ','
                unJsonBuilder $ appendJson el
    BB.appendChar8 ']'

-- FIXME
appendQuote :: Text -> Utf8Builder ()
appendQuote = BB.appendText

appendQuotedString :: Text -> Utf8Builder ()
appendQuotedString txt =
    BB.appendEscapedJson $ DTE.encodeUtf8 txt

(.=) :: ToJson a => Text -> a -> ObjectBuilder
a .= b = ObjectBuilder $ do
    appendQuotedString a
    BB.appendChar8 ':'
    unJsonBuilder $ appendJson b
infixr 8 .=

instance Semigroup ObjectBuilder where
    a <> b = ObjectBuilder $ do
        unObjectBuilder a
        BB.appendChar8 ','
        unObjectBuilder b

instance ToJson ObjectBuilder where
    appendJson ob = JsonBuilder $ do
        BB.appendChar8 '{'
        unObjectBuilder ob
        BB.appendChar8 '}'

instance ToJson Bool where
    appendJson b = JsonBuilder $ BB.unsafeAppendBS $ case b of
        True -> "true"
        False -> "false"

instance ToJson a => ToJson (Maybe a) where
    appendJson m = case m of
        Nothing -> JsonBuilder $ BB.unsafeAppendBS "null"
        Just a -> appendJson a

instance ToJson a => ToJson [a] where
    appendJson = array

instance ToJson Text where
    appendJson = JsonBuilder . appendQuotedString

fromBuilder :: Builder.Builder -> JsonBuilder
fromBuilder builder = JsonBuilder $ BB.unsafeAppendBS $ BSL.toStrict $ Builder.toLazyByteString builder

-- FIXME PERF
instance ToJson Double where
    appendJson d = fromBuilder $ Builder.doubleDec d

instance ToJson Int where
    appendJson d = fromBuilder $ Builder.intDec d
