{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell, ExistentialQuantification #-}

module JsonTest (htf_thisModulesTests) where

import Test.Framework

import Data.Foldable (foldMap)
import qualified Data.Attoparsec.ByteString as Atto
import Data.Monoid ((<>), Monoid (mconcat, mempty))
import Data.String (IsString (..))
import Data.BufferBuilder.Json
import qualified Data.BufferBuilder.Utf8 as BBU
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as JsonParse
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

ae :: (IsString a, Show a, Eq a) => String -> a -> IO ()
ae expected actual = assertEqual (fromString expected) actual

test_encode_int :: IO ()
test_encode_int = do
    ae "-42" (encodeJson (-42::Int))
    ae "9" (encodeJson (9::Int))

test_encode_bool :: IO ()
test_encode_bool = do
    ae "true" (encodeJson True)
    ae "false" (encodeJson False)

test_encode_text :: IO ()
test_encode_text = do
    ae "\"hello\"" (encodeJson ("hello" :: Text))
    ae "\"\"" (encodeJson ("" :: Text))

    ae "\"\\\"\\\\\\n\\r\\t\"" (encodeJson (fromString ['\"', '\\', '\n', '\r', '\t'] :: Text))

    ae "\"\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0007\\u0008\\t\\n\\u000b\\u000c\\r\\u000e\\u000f\\u0010\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0017\\u0018\\u0019\\u001a\\u001b\\u001c\\u001d\\u001e\\u001f\"" (encodeJson (fromString $ map toEnum [1..31] :: Text))

test_encode_object :: IO ()
test_encode_object = do
    ae "{\"key\":\"value\"}" (encodeJson ("key" .= ("value" :: Text)))
    ae "{\"key\":\"value\",\"key2\":[5,6,7]}"
        (encodeJson ("key" .= ("value" :: Text) <> "key2" .= ([5,6,7] :: [Int])))
    ae "{}" $ encodeJson $ ((mempty :: ObjectBuilder) <> mempty) <> (mempty <> mempty)
    ae "[]" (encodeJson ([] :: [Int]))

data CustomThing = CustomThing

encodeCustomThing :: CustomThing -> BBU.Utf8Builder ()
encodeCustomThing _ = do
    BBU.appendChar7 '"'
    BBU.appendBS7 "foo"
    BBU.appendChar7 '/'
    BBU.appendBS7 "bar"
    BBU.appendChar7 '/'
    BBU.appendBS7 "baz"
    BBU.appendChar7 '"'

instance ToJson CustomThing where
    toJson = unsafeValueUtf8Builder . encodeCustomThing
instance ToJsonString CustomThing where
    toJsonString = unsafeStringUtf8Builder . encodeCustomThing

test_encode_object_with_custom_typeclasses :: IO ()
test_encode_object_with_custom_typeclasses = do
    ae "{\"key\":\"value\"}" $ encodeJson $ ("key" :: Text) `row` ("value" :: Text)
    ae "{\"foo/bar/baz\":\"foo/bar/baz\"}" $ encodeJson $ CustomThing `row` CustomThing

test_monoid_laws :: IO ()
test_monoid_laws = do
    -- TODO QuickCheck
    let a = "key" .= ("value" :: Text)
        b = "key2" .= (999 :: Int)
        c = "key3" .= ([1,2,3] :: [Int])
    assertEqual {-Left identity-} (encodeJson a) (encodeJson (mempty <> a))
    assertEqual {-Right identity-} (encodeJson a) (encodeJson (a <> mempty))
    assertEqual {-Associativity-} (encodeJson (a <> (b <> c))) (encodeJson ((a <> b) <> c))
    assertEqual {-mconcat-} (encodeJson (mconcat [a, b, c])) (encodeJson (a <> b <> c))

data JsonValue = forall a. ToJson a => JsonValue a

instance ToJson JsonValue where
    toJson (JsonValue a) = toJson a

instance Show JsonValue where
    show jv = show $ encodeJson jv

shrink10x :: Gen a -> Gen a
shrink10x a = sized $ \size ->
    let newSize = max 1 (size `div` 10)
    in resize newSize a

instance Arbitrary JsonValue where
    arbitrary = do
        i <- fmap (`mod` 9) arbitrary :: Gen Int
        case i of
            0 -> fmap (JsonValue . array)
                (shrink10x arbitrary :: Gen [JsonValue])
            1 -> fmap (JsonValue . Vector.fromList)
                (shrink10x arbitrary :: Gen [JsonValue])
            2 -> fmap (JsonValue . Text.pack) (arbitrary :: Gen String)
            3 -> fmap JsonValue (arbitrary :: Gen Int)
            4 -> fmap JsonValue (arbitrary :: Gen Double)
            5 -> return $ JsonValue True
            6 -> return $ JsonValue False
            7 -> return $ JsonValue (Nothing :: Maybe Int)
            8 -> fmap JsonValue (shrink10x arbitrary :: Gen JsonObject)
            _ -> error "Andy made a mistake"

newtype JsonObject = JsonObject [(Text, JsonValue)]

instance Arbitrary JsonObject where
    arbitrary = do
        p1 <- arbitrary
        return $ JsonObject [(Text.pack k, v) | (k, v) <- p1]

instance ToJson JsonObject where
    toJson (JsonObject pairs) =
        toJson $ foldMap makePair pairs
      where
        makePair (k, v) = k .= v

decodeJsonFragment :: Aeson.FromJSON j => BS.ByteString -> Maybe j
decodeJsonFragment str = case parsed of
    Right r -> case Aeson.fromJSON r of
        Aeson.Success a -> Just a
        _              -> Nothing
    _ -> Nothing
  where
    parsed = Atto.parseOnly JsonParse.value' str

prop_always_produces_legal_json :: JsonValue -> Bool
prop_always_produces_legal_json jv =
    case decodeJsonFragment $ encodeJson jv :: Maybe Aeson.Value of
        Just _ -> True
        Nothing -> False
