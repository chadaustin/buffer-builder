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
    , Value
    , encodeJson

    -- * Objects
    , ObjectBuilder
    , emptyObject
    , (.=)
    , (.=#)
    , pair

    -- * Arrays
    , array

    -- * Null
    , nullValue

    -- * Unsafe
    , unsafeAppendBS
    , unsafeAppendUtf8Builder
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
-- unsafe functions 'unsafeAppendBS' and 'unsafeAppendUtf8Builder' are
-- available.
--
-- Internally, Value encodes an action or sequence of actions that append
-- JSON-encoded text to the underlying 'Utf8Builder'.
newtype Value = Value { utf8Builder :: Utf8Builder () }

---- General JSON value support

-- | Encode a value into a 'ByteString' containing valid UTF-8-encoded JSON.
-- The argument value must have a corresponding 'ToJson' instance.
--
-- __WARNING__: There are three cases where the resulting 'ByteString' may not contain
-- legal JSON:
--
-- * An unsafe function was used to encode a JSON value.
-- * The root value is not an object or array, as the JSON spec requires.
-- * An object has multiple keys with the same value.  For maximum efficiency,
--   'ObjectBuilder' does not check key names for uniqueness, so it's possible to
--   construct objects with duplicate keys.
encodeJson :: ToJson a => a -> ByteString
encodeJson = UB.runUtf8Builder . utf8Builder . toJson
{-# INLINE encodeJson #-}

-- | The class of types that can be converted to JSON values.  See
-- 'ObjectBuilder' for an example of writing a 'ToJson' instance for a
-- custom data type.
class ToJson a where
    toJson :: a -> Value


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

instance Monoid ObjectBuilder where
    {-# INLINE mempty #-}
    mempty = NoPair

    {-# INLINE mappend #-}
    mappend NoPair a = a
    mappend a NoPair = a
    mappend (Pair a) (Pair b) = Pair $ do 
        a
        UB.appendChar7 ','
        b

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
emptyObject = Value $ do
    UB.appendChar7 '{'
    UB.appendChar7 '}'

{-# INLINE writePair #-}
writePair :: ToJson a => (Text, a) -> Utf8Builder ()
writePair (key, value) = do
    UB.appendEscapedJsonText key
    UB.appendChar7 ':'
    utf8Builder $ toJson value

instance ToJson a => ToJson (HashMap.HashMap Text a) where
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

-- | Create an 'ObjectBuilder' from a key and a value.
{-# INLINE (.=) #-}
(.=) :: ToJson a => Text -> a -> ObjectBuilder
a .= b = Pair $ do
    UB.appendEscapedJsonText a
    UB.appendChar7 ':'
    utf8Builder $ toJson b
infixr 8 .=

-- | Wordy alias to '.='.
{-# INLINE pair #-}
pair :: ToJson a => Text -> a -> ObjectBuilder
pair = (.=)
infixr 8 `pair`

-- | Create an 'ObjectBuilder' from a key (expressed as an 'Addr#') and a value
{-# INLINE (.=#) #-}
(.=#) :: ToJson a => Addr# -> a -> ObjectBuilder
a .=# b = Pair $ do
    UB.appendEscapedJsonLiteral a
    UB.appendChar7 ':'
    utf8Builder $ toJson b
infixr 8 .=#

---- Arrays

-- | Serialize a 'Foldable' as a JSON array.
{-# INLINABLE array #-}
array :: (Foldable t, ToJson a) => t a -> Value
array collection = Value $ do
    UB.appendChar7 '['
    -- HACK: ObjectBuilder is not "type correct" but it has exactly the behaviour we want for this function.
    case foldMap (Pair . utf8Builder . toJson) collection of
        NoPair -> return ()
        (Pair b) -> b
    UB.appendChar7 ']'

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
    toJson txt = Value $ UB.appendEscapedJsonText txt

instance ToJson Double where
    {-# INLINE toJson #-}
    toJson a = Value $ UB.appendDecimalDouble a

instance ToJson Int where
    {-# INLINE toJson #-}
    toJson a = Value $ UB.appendDecimalSignedInt a

-- | Unsafely append a string into a JSON document.
-- This function does /not/ escape, quote, or otherwise decorate the string in any way.
-- This function is /unsafe/ because you can trivially use it to generate illegal JSON.
unsafeAppendBS :: ByteString -> Value
unsafeAppendBS bs = Value $ UB.unsafeAppendBS bs

-- | Unsafely append a 'Utf8Builder' into a JSON document.
-- This function does not escape, quote, or decorate the string in any way.
-- This function is /unsafe/ because you can trivially use it to generate illegal JSON.
unsafeAppendUtf8Builder :: Utf8Builder () -> Value
unsafeAppendUtf8Builder utf8b = Value utf8b

-- | Represents a JSON "null".
nullValue :: Value
nullValue = Value $ UB.unsafeAppendLiteralN 4 "null"#
