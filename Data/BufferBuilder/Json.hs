{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.BufferBuilder.Json
    ( -- * JSON
      -- $use
      ToJson (..)
    , JsonBuilder
    , ObjectBuilder
    , encodeJson
    , emptyObject
    , (.=)
    ) where

import           Data.BufferBuilder.Utf8 (Utf8Builder)
import qualified Data.BufferBuilder.Utf8 as BB
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as Builder
import           Data.Foldable (for_)
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text.Encoding as DTE

-- | Builds a JSON value.
--
-- 'JsonBuilder's are built up either with '.=' and 'Data.Semigroup.<>' or from other 'ToJson' instances.
--
newtype JsonBuilder   = JsonBuilder { unJsonBuilder :: Utf8Builder () }

-- | Builds a JSON object.
--
-- An 'ObjectBuilder' builds one or more key-value pairs of a JSON object.  They are constructed with the '.=' operator and
-- combined with 'Data.Semigroup.<>'.
--
-- To turn an 'ObjectBuilder' into a 'JsonBuilder', use its 'ToJson' class instance.
--
-- @
--     data Friend = Friend
--         { fId :: !Int
--         , fName :: !Text
--         } deriving (Eq, Show)
--
--     instance ToJson Friend where
--         appendJson friend = appendJson $
--                    "id"   .= fId friend
--                 <> "name" .= fName friend
-- @
newtype ObjectBuilder = ObjectBuilder { unObjectBuilder :: Utf8Builder () }

-- | Run a builder and get the resulting JSON.
-- With this function, you can use 'JsonBuilders' that were not obtained by using the 'ToJson' typeclass.
-- This is useful if you want to be able to JSON-encode the same data type in multiple ways.
runBuilder :: JsonBuilder -> ByteString
runBuilder = BB.runUtf8Builder . unJsonBuilder

-- | Convert a datum to JSON.
-- Eqivalent to
-- @
--     runBuilder . appendJson
-- @
encodeJson :: ToJson a => a -> ByteString
encodeJson = runBuilder . appendJson

-- | The class of types that can be converted to JSON.
class ToJson a where
    appendJson :: a -> JsonBuilder

-- | A 'JsonBuilder' that represents the empty object.
-- 'JsonBuilder' is a 'Semigroup', but if it were a 'Data.Monoid.Monoid', this would be 'Data.Monoid.mempty'.
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

{-# INLINE appendQuotedString #-}
appendQuotedString :: Text -> Utf8Builder ()
appendQuotedString txt =
    BB.appendEscapedJson $ DTE.encodeUtf8 txt

-- | Create an 'ObjectBuilder' from a key and a value.
{-# INLINE (.=) #-}
(.=) :: ToJson a => Text -> a -> ObjectBuilder
a .= b = ObjectBuilder $ do
    appendQuotedString a
    BB.appendChar8 ':'
    unJsonBuilder $ appendJson b
infixr 8 .=

instance Semigroup ObjectBuilder where
    {-# INLINE (<>) #-}
    a <> b = ObjectBuilder $ do
        unObjectBuilder a
        BB.appendChar8 ','
        unObjectBuilder b

instance ToJson ObjectBuilder where
    {-# INLINE appendJson #-}
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
    {-# INLINE appendJson #-}
    appendJson = array

instance ToJson Text where
    {-# INLINE appendJson #-}
    appendJson = JsonBuilder . appendQuotedString

fromBuilder :: Builder.Builder -> JsonBuilder
fromBuilder builder = JsonBuilder $ BB.unsafeAppendBS $ BSL.toStrict $ Builder.toLazyByteString builder

-- FIXME PERF
instance ToJson Double where
    {-# INLINE appendJson #-}
    appendJson a = JsonBuilder $ BB.appendDecimalDouble a

instance ToJson Int where
    {-# INLINE appendJson #-}
    appendJson a = JsonBuilder $ BB.appendDecimalSignedInt a
