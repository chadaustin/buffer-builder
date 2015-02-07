{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell #-}

module JsonTest where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
import Data.String (IsString (..))
import Data.Text (Text)
import Data.BufferBuilder.Json
-- import Data.BufferBuilder.Json.Aeson
-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Lazy as BSL
-- import AesonQuickCheck ()

ae expected actual = assertEqual expected (fromString expected) actual

case_encode_int = do
    ae "-42" (encodeJson (-42::Int))
    ae "9" (encodeJson (9::Int))

case_encode_bool = do
    ae "true" (encodeJson True)
    ae "false" (encodeJson False)

case_encode_text = do
    ae "\"hello\"" (encodeJson ("hello" :: Text))
    ae "\"\"" (encodeJson ("" :: Text))

    ae "\"\\\"\\\\\\n\\r\\t\"" (encodeJson (fromString ['\"', '\\', '\n', '\r', '\t'] :: Text))

-- prop_is_same_as_aeson :: Aeson.Value -> Bool
-- prop_is_same_as_aeson document =
--     BSL.toStrict (Aeson.encode document) == encodeJson document

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
