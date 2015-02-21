{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns, UndecidableInstances #-}

{-|
A library for efficiently building up a valid JSON document.

This module is built on top of 'Data.Utf8Builder'.
-}
module Data.BufferBuilder.Json
    (
    -- * Encoding Values
      ToJson (..)
    , JsonBuilder
    , encodeJson

    -- * Objects
    , ObjectBuilder
    , emptyObject
    , (.=)
    , (.=#)
    , pair

    -- * Arrays
    , array
    , unsafeAppendBS
    , unsafeAppendUtf8Builder
    , nullValue
    ) where

import GHC.Base
import Foreign.Storable
import Control.Monad (when, forM_)
import Data.BufferBuilder.Utf8 (Utf8Builder)
import qualified Data.BufferBuilder.Utf8 as UB
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Foldable (Foldable, foldMap)
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GVector
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HashMap

-- | Builds a JSON value.
--
-- 'Value's are built up from either 'ToJson' instances or
-- from primitives like 'emptyObject', 'array', and 'null'.
--
-- In special cases, or when  or the unsafe functions 'unsafeAppendBS' or
-- 'unsafeAppendUtf8Builder'.
--
newtype JsonBuilder = JsonBuilder { utf8Builder :: Utf8Builder () }

---- General JSON value support

-- | Encode a value as a 'ByteString' containing valid UTF-8-encoded JSON.
-- The value must have a corresponding 'ToJson' instance.
--
-- __WARNING__: There are three cases where the 'ByteString' will not contain
-- legal JSON:
--
-- * An unsafe function was used to encode a JSON value.
-- * The root value is not an object or array, as the JSON spec requires.
-- * An object has multiple keys with the same value.  For maximum efficiency,
--   'ObjectBuilder' does not check key names for uniqueness, so it's possible to
--   construct objects with duplicate keys.
encodeJson :: ToJson a => a -> ByteString
encodeJson = UB.runUtf8Builder . utf8Builder . appendJson
{-# INLINE encodeJson #-}

-- | The class of types that can be converted to JSON.
class ToJson a where
    appendJson :: a -> JsonBuilder


---- Objects

-- | Builds a JSON object.
--
-- An 'ObjectBuilder' builds one or more key-value pairs of a JSON object.  They are constructed with the '.=' operator and
-- combined with 'Data.Monoid.<>'.
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
data ObjectBuilder = ObjectBuilder
    { unObjectBuilder :: Utf8Builder ()
    , needsComma :: !Int
    }

instance Monoid ObjectBuilder where
    {-# INLINE mempty #-}
    mempty = ObjectBuilder (return ()) 0

    {-# INLINE mappend #-}
    mappend a b = ObjectBuilder go 1
      where
        go = do
            unObjectBuilder a
            when (2 == needsComma a + needsComma b) $
                UB.appendChar7 ','
            unObjectBuilder b

instance ToJson ObjectBuilder where
    {-# INLINE appendJson #-}
    appendJson ob = JsonBuilder $ do
        UB.appendChar7 '{'
        unObjectBuilder ob
        UB.appendChar7 '}'

-- | A 'JsonBuilder' that produces the empty object.
emptyObject :: JsonBuilder
emptyObject = JsonBuilder $ do
    UB.appendChar7 '{'
    UB.appendChar7 '}'

{-# INLINE writePair #-}
writePair :: ToJson a => (Text, a) -> Utf8Builder ()
writePair (key, value) = do
    UB.appendEscapedJsonText key
    UB.appendChar7 ':'
    utf8Builder $ appendJson value

instance ToJson a => ToJson (HashMap.HashMap Text a) where
    {-# INLINABLE appendJson #-}
    appendJson hm = JsonBuilder $ do
        UB.appendChar7 '{'
        case HashMap.toList hm of
            [] -> UB.appendChar7 '}'
            (x:xs) -> do
                writePair x
                forM_ xs $ \p -> do
                    UB.appendChar7 ','
                    writePair p
                UB.appendChar7 '}'

-- | Create an 'ObjectBuilder' from a key and a value.
{-# INLINE (.=) #-}
(.=) :: ToJson a => Text -> a -> ObjectBuilder
a .= b = ObjectBuilder go 1
  where
    go = do
        UB.appendEscapedJsonText a
        UB.appendChar7 ':'
        utf8Builder $ appendJson b
infixr 8 .=

-- | Wordy alias to '.='.
{-# INLINE pair #-}
pair :: ToJson a => Text -> a -> ObjectBuilder
pair = (.=)
infixr 8 `pair`

-- | Create an 'ObjectBuilder' from a key (expressed as an 'Addr#') and a value
{-# INLINE (.=#) #-}
(.=#) :: ToJson a => Addr# -> a -> ObjectBuilder
a .=# b = ObjectBuilder go 1
  where
    go = do
        UB.appendEscapedJsonLiteral a
        UB.appendChar7 ':'
        utf8Builder $ appendJson b
infixr 8 .=#

---- Arrays

-- | Serialize a 'Foldable' as a JSON array.
{-# INLINABLE array #-}
array :: (Foldable t, ToJson a) => t a -> JsonBuilder
array collection = JsonBuilder $ do
    UB.appendChar7 '['
    -- HACK: ObjectBuilder is not "type correct" but it has exactly the behaviour we want for this function.
    unObjectBuilder $ foldMap (\e -> ObjectBuilder (utf8Builder $ appendJson e) 1) collection
    UB.appendChar7 ']'

instance ToJson a => ToJson [a] where
    {-# INLINABLE appendJson #-}
    appendJson !ls = JsonBuilder $ do
        UB.appendChar7 '['
        case ls of
            [] -> UB.appendChar7 ']'
            x:xs -> do
                utf8Builder $ appendJson x
                forM_ xs $ \(!e) -> do
                    UB.appendChar7 ','
                    utf8Builder $ appendJson e
                UB.appendChar7 ']'

instance ToJson a => ToJson (Vector.Vector a) where
    {-# INLINABLE appendJson #-}
    appendJson = vector

instance (Storable a, ToJson a) => ToJson (VS.Vector a) where
    {-# INLINABLE appendJson #-}
    appendJson = vector

instance (VP.Prim a, ToJson a) => ToJson (VP.Vector a) where
    {-# INLINABLE appendJson #-}
    appendJson = vector

instance (GVector.Vector VU.Vector a, ToJson a) => ToJson (VU.Vector a) where
    {-# INLINABLE appendJson #-}
    appendJson = vector

{-# INLINABLE vector #-}
vector :: (GVector.Vector v a, ToJson a) => v a -> JsonBuilder
vector !vec = JsonBuilder $ do
    UB.appendChar7 '['
    let len = GVector.length vec
    when (len /= 0) $ do
        utf8Builder $ appendJson (vec `GVector.unsafeIndex` 0)
        GVector.forM_ (GVector.tail vec) $ \e -> do
            UB.appendChar7 ','
            utf8Builder $ appendJson e
    UB.appendChar7 ']'


---- Common JSON instances

instance ToJson JsonBuilder where
    {-# INLINE appendJson #-}
    appendJson = id

instance ToJson Bool where
    {-# INLINE appendJson #-}
    appendJson True = JsonBuilder $ UB.unsafeAppendLiteralN 4 "true"#
    appendJson False = JsonBuilder $ UB.unsafeAppendLiteralN 5 "false"#

instance ToJson a => ToJson (Maybe a) where
    {-# INLINE appendJson #-}
    appendJson m = case m of
        Nothing -> JsonBuilder $ UB.unsafeAppendLiteralN 4 "null"#
        Just a -> appendJson a

instance ToJson Text where
    {-# INLINE appendJson #-}
    appendJson txt = JsonBuilder $ UB.appendEscapedJsonText txt

instance ToJson Double where
    {-# INLINE appendJson #-}
    appendJson a = JsonBuilder $ UB.appendDecimalDouble a

instance ToJson Int where
    {-# INLINE appendJson #-}
    appendJson a = JsonBuilder $ UB.appendDecimalSignedInt a

-- | Unsafely append a string into a JSON document.
-- This function does _not_ escape, quote, or otherwise decorate the string in any way.
-- This function is _unsafe_ because you can trivially use it to generate illegal JSON.
unsafeAppendBS :: ByteString -> JsonBuilder
unsafeAppendBS bs = JsonBuilder $ UB.unsafeAppendBS bs

-- | Unsafely append a 'Utf8Builder' into a JSON document.
-- This function does not escape, quote, or decorate the string in any way.
-- This function is _unsafe_ because you can trivially use it to generate illegal JSON.
unsafeAppendUtf8Builder :: Utf8Builder () -> JsonBuilder
unsafeAppendUtf8Builder utf8b = JsonBuilder utf8b

-- | Build a JSON "null".
nullValue :: JsonBuilder
nullValue = JsonBuilder $ UB.unsafeAppendLiteralN 4 "null"#
