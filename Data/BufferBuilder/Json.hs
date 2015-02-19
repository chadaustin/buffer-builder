{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.BufferBuilder.Json
    ( -- * JSON
      ToJson (..)
    , JsonBuilder
    , ObjectBuilder
    , encodeJson
    , runBuilder
    , emptyObject
    , hashMap
    , (.=)
    , (.=#)
    , pair
    , array
    , list
    , vector
    , unsafeAppendBS
    , unsafeAppendUtf8Builder
    , appendNull
    ) where

import GHC.Base
import Control.Monad (when, forM_)
import Data.BufferBuilder.Utf8 (Utf8Builder)
import qualified Data.BufferBuilder.Utf8 as UB
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Foldable (Foldable, foldMap)
import qualified Data.Vector.Generic as GVector
import qualified Data.HashMap.Strict as HashMap

-- | Builds a JSON value.
--
-- 'JsonBuilder's are built up either with '.=' and 'Data.Monoid.<>' or from other 'ToJson' instances.
--
newtype JsonBuilder = JsonBuilder { unJsonBuilder :: Utf8Builder () }

-- | Run a builder and get the resulting JSON.
-- With this function, you can use 'JsonBuilders' that were not obtained by using the 'ToJson' typeclass.
-- This is useful if you want to be able to JSON-encode the same data type in multiple ways.
runBuilder :: JsonBuilder -> ByteString
runBuilder = UB.runUtf8Builder . unJsonBuilder


---- General JSON vValue support

-- | The class of types that can be converted to JSON.
class ToJson a where
    appendJson :: a -> JsonBuilder

-- | Convert a datum to JSON.
-- Eqivalent to
-- @
--     runBuilder . appendJson
-- @
encodeJson :: ToJson a => a -> ByteString
encodeJson = runBuilder . appendJson


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

-- | A 'JsonBuilder' that represents the empty object.
emptyObject :: JsonBuilder
emptyObject = JsonBuilder $ do
    UB.appendChar7 '{'
    UB.appendChar7 '}'

{-# INLINE writePair #-}
writePair :: ToJson a => (Text, a) -> Utf8Builder ()
writePair (key, value) = do
    UB.appendEscapedJsonText key
    UB.appendChar7 ':'
    unJsonBuilder $ appendJson value

{-# INLINABLE hashMap #-}
hashMap :: ToJson a => HashMap.HashMap Text a -> JsonBuilder
hashMap hm = JsonBuilder $ do
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
        unJsonBuilder $ appendJson b
infixr 8 .=

{-# INLINE (.=#) #-}
(.=#) :: ToJson a => Addr# -> a -> ObjectBuilder
a .=# b = ObjectBuilder go 1
  where
    go = do
        UB.appendEscapedJsonLiteral a
        UB.appendChar7 ':'
        unJsonBuilder $ appendJson b

-- | Wordy alias to '.='.
{-# INLINE pair #-}
pair :: ToJson a => Text -> a -> ObjectBuilder
pair = (.=)
infixr 8 `pair`


---- Arrays

-- | Serialize a 'Foldable' as a JSON array.
{-# INLINABLE array #-}
array :: (Foldable t, ToJson a) => t a -> JsonBuilder
array collection = JsonBuilder $ do
    UB.appendChar7 '['
    -- HACK: ObjectBuilder is not "type correct" but it has exactly the behaviour we want for this function.
    unObjectBuilder $ foldMap (\e -> ObjectBuilder (unJsonBuilder $ appendJson e) 1) collection
    UB.appendChar7 ']'

{-# INLINABLE list #-}
list :: ToJson a => [a] -> JsonBuilder
list !ls = JsonBuilder $ do
    UB.appendChar7 '['
    case ls of
        [] -> UB.appendChar7 ']'
        x:xs -> do
            unJsonBuilder $ appendJson x
            forM_ xs $ \(!e) -> do
                UB.appendChar7 ','
                unJsonBuilder $ appendJson e
            UB.appendChar7 ']'

-- | Serialize a 'Data.Vector.Generic.Vector' as a JSON array.
-- This function generates better code than 'array', particularly for unboxed vectors.
{-# INLINABLE vector #-}
vector :: (GVector.Vector v a, ToJson a) => v a -> JsonBuilder
vector !vec = JsonBuilder $ do
    UB.appendChar7 '['
    let len = GVector.length vec
    when (len /= 0) $ do
        unJsonBuilder $ appendJson (vec `GVector.unsafeIndex` 0)
        GVector.forM_ (GVector.tail vec) $ \e -> do
            UB.appendChar7 ','
            unJsonBuilder $ appendJson e
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

instance ToJson a => ToJson [a] where
    {-# INLINE appendJson #-}
    appendJson = list

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

appendNull :: JsonBuilder
appendNull = JsonBuilder $ UB.unsafeAppendLiteralN 4 "null"#
