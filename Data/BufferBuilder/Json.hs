{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.BufferBuilder.Json
    ( -- * JSON
      -- $use
      ToJson (..)
    , JsonBuilder
    , ObjectBuilder
    , encodeJson
    , runBuilder
    , emptyObject
    , (.=)
    , (.=#)
    , pair
    , array
    , vector
    , unsafeAppendBS
    , appendNull
    ) where

import           GHC.Base
import           Control.Monad (when)
import           Data.BufferBuilder.Utf8 (Utf8Builder)
import qualified Data.BufferBuilder.Utf8 as BB
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Foldable (Foldable, foldMap)
import qualified Data.Vector.Generic as GVector

-- | Builds a JSON value.
--
-- 'JsonBuilder's are built up either with '.=' and 'Data.Monoid.<>' or from other 'ToJson' instances.
--
newtype JsonBuilder = JsonBuilder { unJsonBuilder :: Utf8Builder () }

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
emptyObject :: JsonBuilder
emptyObject = JsonBuilder $ do
    BB.appendChar8 '{'
    BB.appendChar8 '}'

-- | Create an 'ObjectBuilder' from a key and a value.
{-# INLINE (.=) #-}
(.=) :: ToJson a => Text -> a -> ObjectBuilder
a .= b = ObjectBuilder go 1
  where
    go = do
        BB.appendEscapedJsonText a
        BB.appendChar8 ':'
        unJsonBuilder $ appendJson b
infixr 8 .=

{-# INLINE (.=#) #-}
(.=#) :: ToJson a => Addr# -> a -> ObjectBuilder
a .=# b = ObjectBuilder go 1
  where
    go = do
        BB.appendEscapedJsonLiteral a
        BB.appendChar8 ':'
        unJsonBuilder $ appendJson b

-- | Wordy alias to '.='.
{-# INLINE pair #-}
pair :: ToJson a => Text -> a -> ObjectBuilder
pair = (.=)
infixr 8 `pair`

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
                BB.appendChar8 ','
            unObjectBuilder b

instance ToJson ObjectBuilder where
    {-# INLINE appendJson #-}
    appendJson ob = JsonBuilder $ do
        BB.appendChar8 '{'
        unObjectBuilder ob
        BB.appendChar8 '}'

-- | Serialize a 'Foldable' as a JSON array.
array :: (Foldable t, ToJson a) => t a -> JsonBuilder
array collection = JsonBuilder $ do
    BB.appendChar8 '['
    -- HACK: ObjectBuilder is not "type correct" but it has exactly the behaviour we want for this function.
    unObjectBuilder $ foldMap (\e -> ObjectBuilder (unJsonBuilder $ appendJson e) 1) collection
    BB.appendChar8 ']'

-- | Serialize a 'Data.Vector.Generic.Vector' as a JSON array.
-- This function generates better code than 'array', particularly for unboxed vectors.
{-# INLINABLE vector #-}
vector :: (GVector.Vector v a, ToJson a) => v a -> JsonBuilder
vector !vec = JsonBuilder $ do
    BB.appendChar8 '['
    let len = GVector.length vec
    when (len /= 0) $ do
        unJsonBuilder $ appendJson (vec `GVector.unsafeIndex` 0)
        GVector.forM_ (GVector.slice 1 (len - 1) vec) $ \e -> do
            BB.appendChar8 ','
            unJsonBuilder $ appendJson e
    BB.appendChar8 ']'

instance ToJson Bool where
    {-# INLINE appendJson #-}
    appendJson True = JsonBuilder $ BB.unsafeAppendLiteralN 4 "true"#
    appendJson False = JsonBuilder $ BB.unsafeAppendLiteralN 5 "false"#

instance ToJson a => ToJson (Maybe a) where
    appendJson m = case m of
        Nothing -> unsafeAppendBS "null"
        Just a -> appendJson a

instance ToJson a => ToJson [a] where
    {-# INLINE appendJson #-}
    appendJson = array

instance ToJson Text where
    {-# INLINE appendJson #-}
    appendJson txt = JsonBuilder $ BB.appendEscapedJsonText txt

instance ToJson Double where
    {-# INLINE appendJson #-}
    appendJson a = JsonBuilder $ BB.appendDecimalDouble a

instance ToJson Int where
    {-# INLINE appendJson #-}
    appendJson a = JsonBuilder $ BB.appendDecimalSignedInt a

-- | Unsafely append a string into a JSON document.
-- This function does _not_ escape, quote, or otherwise decorate the string in any way.
-- This function is _unsafe_ because you can trivially use it to generate illegal JSON.
unsafeAppendBS :: ByteString -> JsonBuilder
unsafeAppendBS bs = JsonBuilder $ BB.unsafeAppendBS bs

appendNull :: JsonBuilder
appendNull = JsonBuilder $ BB.unsafeAppendLiteralN 4 "null"#
