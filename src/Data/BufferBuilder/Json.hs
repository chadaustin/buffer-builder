{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns, UndecidableInstances #-}

{-|
A library for efficiently building up a valid JSON document.

The difference between "Data.BufferBuilder.Json" and the excellent
"Data.Aeson" is that Aeson represents the JSON document as an
in-memory tree structure before encoding it into bytes.  This module,
on the other hand, represents each value as an action that writes its
representation directly into the output buffer.  At the cost of
reduced flexibility, this results in significantly improved encoding
performance.  At the time of this writing, encoding a custom record
type into JSON using this module was almost 5x faster than using
Aeson.

This module is built on top of "Data.Utf8Builder".
-}
module Data.BufferBuilder.Json
    (
    -- * Encoding Values
      Value
    , ToJson (..)
    , encodeJson

    -- * Encoding Strings
    , JsonString
    , ToJsonString (..)

    -- * Objects
    , ObjectBuilder
    , emptyObject
    , (.=)
    , (.=#)
    , row

    -- * Arrays
    , array

    -- * Null
    , nullValue

    -- * Unsafe
    , unsafeValueUtf8Builder
    , unsafeStringUtf8Builder

    -- * Deprecated
    , unsafeAppendUtf8Builder
    , unsafeAppendBS
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

-- | Represents a JSON value.
--
-- 'Value's are built up from either 'ToJson' instances or
-- from primitives like 'emptyObject', 'array', and 'null'.
--
-- In special cases, or when performance is of utmost importance, the
-- unsafe functions 'unsafeAppendUtf8Builder' are
-- available.
--
-- Internally, Value encodes an action or sequence of actions that append
-- JSON-encoded text to the underlying 'Utf8Builder'.
newtype Value = Value { utf8Builder :: Utf8Builder () }

-- | The class of types that can be converted to JSON values.  See
-- 'ObjectBuilder' for an example of writing a 'ToJson' instance for a
-- custom data type.
--
-- 'ToJson' instances are provided for many common types.  For
-- example, to create a JSON array, call 'toJson' on a list or 'Vector.Vector'.
-- To create a JSON object, call 'toJson' on a 'HashMap.HashMap'.
class ToJson a where
    toJson :: a -> Value

---- General JSON value support

-- | Encode a value into a 'ByteString' containing valid UTF-8-encoded JSON.
-- The argument value must have a corresponding 'ToJson' instance.
--
-- __WARNING__: There are three cases where the resulting 'ByteString' may not contain
-- legal JSON:
--
-- * An unsafe function was used to encode a JSON value.
-- * The root value is not an object or array, as the JSON specification requires.
-- * An object has multiple keys with the same value.  For maximum efficiency,
--   'ObjectBuilder' does not check key names for uniqueness, so it's possible to
--   construct objects with duplicate keys.
encodeJson :: ToJson a => a -> ByteString
encodeJson = UB.runUtf8Builder . utf8Builder . toJson
{-# INLINE encodeJson #-}

---- String

-- | Represents a JSON string.
newtype JsonString = JsonString { unJsonString :: Utf8Builder () }

-- | The class of types that can be converted to JSON strings.  Any
-- type that provides ToJsonString also provides ToJson, and thus can
-- be used as JSON values.
class ToJson a => ToJsonString a where
    toJsonString :: a -> JsonString

---- Objects

-- | Builds a JSON object.
--
-- An 'ObjectBuilder' builds one or more key-value pairs of a JSON object.  They are constructed with the '.=' operator and
-- combined with 'Data.Monoid.<>'.
--
-- To turn an 'ObjectBuilder' into a 'Value', use its 'ToJson' class instance.
--
-- @
--     data Friend = Friend
--         { fId :: !Int
--         , fName :: !Text
--         } deriving (Eq, Show)
--
--     instance ToJson Friend where
--         toJson friend = toJson $
--                    "id"   .= fId friend
--                 <> "name" .= fName friend
-- @
--
-- __WARNING__: 'ObjectBuilder' does not check uniqueness of object
-- keys.  If two keys with the same value are inserted, then the
-- resulting JSON document will be illegal.
data ObjectBuilder = NoPair | Pair !(Utf8Builder ())

instance Semigroup ObjectBuilder where
    {-# INLINE (<>) #-}
    NoPair <> a = a
    a <> NoPair = a
    (Pair a) <> (Pair b) = Pair $ do
        a
        UB.appendChar7 ','
        b

instance Monoid ObjectBuilder where
    {-# INLINE mempty #-}
    mempty = NoPair

    {-# INLINE mappend #-}
    mappend = (<>)

instance ToJson ObjectBuilder where
    {-# INLINE toJson #-}
    toJson NoPair = Value $ do
        UB.appendChar7 '{'
        UB.appendChar7 '}'

    toJson (Pair a) = Value $ do
        UB.appendChar7 '{'
        a
        UB.appendChar7 '}'

-- | A 'Value' that produces the empty object.
emptyObject :: Value
emptyObject = toJson NoPair
{-# INLINE emptyObject #-}

-- | Create an ObjectBuilder from an arbitrary key and value.  The key can be any
-- type with a 'ToJsonString' instance.
row :: (ToJsonString k, ToJson v) => k -> v -> ObjectBuilder
row k v = Pair $ do
    unJsonString $ toJsonString k
    UB.appendChar7 ':'
    utf8Builder $ toJson v
infixr 8 `row`
{-# INLINE row #-}

-- | Create an 'ObjectBuilder' from a key and a value.
(.=) :: ToJson a => Text -> a -> ObjectBuilder
a .= b = Pair $ do
    UB.appendEscapedJsonText a
    UB.appendChar7 ':'
    utf8Builder $ toJson b
infixr 8 .=
{-# INLINE (.=) #-}

-- | Create an 'ObjectBuilder' from a key and a value.  The key is an
-- ASCII-7, unescaped, zero-terminated 'Addr#'.
--
-- __WARNING__: This function is unsafe.  If the key is NOT
-- zero-terminated, then an access violation might result.  If the key
-- is not a sequence of unescaped ASCII characters, the resulting JSON
-- document will be illegal.
--
-- This function is provided for maximum performance in the common
-- case that object keys are ASCII-7.  It achieves performance by
-- avoiding the CAF for a Text literal and avoiding the need to
-- transcode UTF-16 to UTF-8 and escape.
--
-- To use this function, the calling source file must have the
-- MagicHash extension enabled.
--
-- @
--     data Friend = Friend
--         { fId :: !Int
--         , fName :: !Text
--         } deriving (Eq, Show)
--
--     instance ToJson Friend where
--         toJson friend = toJson $
--                    "id"\#   .=\# fId friend
--                 <> "name"\# .=\# fName friend
-- @
(.=#) :: ToJson a => Addr# -> a -> ObjectBuilder
a .=# b = Pair $ do
    UB.appendEscapedJsonLiteral a
    UB.appendChar7 ':'
    utf8Builder $ toJson b
infixr 8 .=#
{-# INLINE (.=#) #-}


{-# INLINE writePair #-}
writePair :: (ToJsonString k, ToJson v) => (k, v) -> Utf8Builder ()
writePair (key, value) = do
    unJsonString $ toJsonString key
    UB.appendChar7 ':'
    utf8Builder $ toJson value

instance (ToJsonString k, ToJson v) => ToJson (HashMap.HashMap k v) where
    {-# INLINABLE toJson #-}
    toJson hm = Value $ do
        UB.appendChar7 '{'
        case HashMap.toList hm of
            [] -> UB.appendChar7 '}'
            (x:xs) -> do
                writePair x
                forM_ xs $ \p -> do
                    UB.appendChar7 ','
                    writePair p
                UB.appendChar7 '}'

---- Arrays

-- | Serialize any 'Foldable' as a JSON array.  This is generally
-- slower than directly calling 'toJson' on a list or 'Vector.Vector',
-- but it will convert any 'Foldable' type into an array.
array :: (Foldable t, ToJson a) => t a -> Value
array collection = Value $ do
    UB.appendChar7 '['
    -- HACK: ObjectBuilder is not "type correct" but it has exactly the behaviour we want for this function.
    case foldMap (Pair . utf8Builder . toJson) collection of
        NoPair -> return ()
        (Pair b) -> b
    UB.appendChar7 ']'
{-# INLINABLE array #-}

instance ToJson a => ToJson [a] where
    {-# INLINABLE toJson #-}
    toJson !ls = Value $ do
        UB.appendChar7 '['
        case ls of
            [] -> UB.appendChar7 ']'
            x:xs -> do
                utf8Builder $ toJson x
                forM_ xs $ \(!e) -> do
                    UB.appendChar7 ','
                    utf8Builder $ toJson e
                UB.appendChar7 ']'

instance ToJson a => ToJson (Vector.Vector a) where
    {-# INLINABLE toJson #-}
    toJson = vector

instance (Storable a, ToJson a) => ToJson (VS.Vector a) where
    {-# INLINABLE toJson #-}
    toJson = vector

instance (VP.Prim a, ToJson a) => ToJson (VP.Vector a) where
    {-# INLINABLE toJson #-}
    toJson = vector

instance (GVector.Vector VU.Vector a, ToJson a) => ToJson (VU.Vector a) where
    {-# INLINABLE toJson #-}
    toJson = vector

{-# INLINABLE vector #-}
vector :: (GVector.Vector v a, ToJson a) => v a -> Value
vector !vec = Value $ do
    UB.appendChar7 '['
    let len = GVector.length vec
    when (len /= 0) $ do
        utf8Builder $ toJson (vec `GVector.unsafeIndex` 0)
        GVector.forM_ (GVector.tail vec) $ \e -> do
            UB.appendChar7 ','
            utf8Builder $ toJson e
    UB.appendChar7 ']'


----

-- | Represents a JSON "null".
nullValue :: Value
nullValue = Value $ UB.unsafeAppendLiteralN 4 "null"#
{-# INLINE nullValue #-}

---- Common JSON instances

instance ToJson Value where
    {-# INLINE toJson #-}
    toJson = id

instance ToJson Bool where
    {-# INLINE toJson #-}
    toJson True = Value $ UB.unsafeAppendLiteralN 4 "true"#
    toJson False = Value $ UB.unsafeAppendLiteralN 5 "false"#

instance ToJson a => ToJson (Maybe a) where
    {-# INLINE toJson #-}
    toJson m = case m of
        Nothing -> Value $ UB.unsafeAppendLiteralN 4 "null"#
        Just a -> toJson a

instance ToJson Text where
    {-# INLINE toJson #-}
    toJson text = Value $ UB.appendEscapedJsonText text

instance ToJson Double where
    {-# INLINE toJson #-}
    toJson a = Value $ UB.appendDecimalDouble a

instance ToJson Int where
    {-# INLINE toJson #-}
    toJson a = Value $ UB.appendDecimalSignedInt a

instance ToJsonString JsonString where
    toJsonString = id

instance ToJson JsonString where
    toJson = Value . unJsonString

instance ToJsonString Text where
    {-# INLINE toJsonString #-}
    toJsonString text = JsonString $ UB.appendEscapedJsonText text


---- Unsafe functions

-- | Unsafely convert a 'Utf8Builder' into a JSON value.
-- This function does not escape, quote, or decorate the string in any way.
-- This function is /unsafe/ because you can trivially use it to generate illegal JSON.
unsafeValueUtf8Builder :: Utf8Builder () -> Value
unsafeValueUtf8Builder = Value

-- | Unsafely convert a 'Utf8Builder' into a JSON string.
-- This function does not escape, quote, or decorate the string in any way.
-- This function is /unsafe/ because you can trivially use it to generate illegal JSON.
unsafeStringUtf8Builder :: Utf8Builder () -> JsonString
unsafeStringUtf8Builder = JsonString


---- Deprecated

{-# DEPRECATED unsafeAppendBS "Use unsafeValueUtf8Builder or unsafeStringUtf8Builder instead" #-}
unsafeAppendBS :: ByteString -> Value
unsafeAppendBS bs = Value $ UB.unsafeAppendBS bs

{-# DEPRECATED unsafeAppendUtf8Builder "Use unsafeValueUtf8Builder or unsafeStringUtf8Builder instead" #-}
unsafeAppendUtf8Builder :: Utf8Builder () -> Value
unsafeAppendUtf8Builder = unsafeValueUtf8Builder
