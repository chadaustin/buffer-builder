{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BufferTest (tests, main) where

import qualified Data.ByteString.Char8 as BSC
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.BufferBuilder
import Test.QuickCheck.Instances ()

case_append_bytes :: Assertion
case_append_bytes = do
    let result = runBufferBuilder $ do
            appendChar8 'f'
            appendChar8 'o'
            appendChar8 'o'
    assertEqual "matches" "foo" result

case_append_string :: Assertion
case_append_string = do
    let result = runBufferBuilder $ do
            appendChar8 'f'
            appendChar8 'o'
            appendChar8 'o'
            appendBS "bar"
    assertEqual "matches" "foobar" result

case_append_literals :: Assertion
case_append_literals = do
    let result = runBufferBuilder $ do
            appendLiteral "foo"#
            appendLiteral "bar"#
    assertEqual "matches" "foobar" result

case_append_url_encoded_safe :: Assertion
case_append_url_encoded_safe = do
    let safe = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"
    let result = runBufferBuilder $ do
            appendUrlEncoded safe
    assertEqual "matches" safe result

case_append_url_encoded_unsafe :: Assertion
case_append_url_encoded_unsafe = do
    let safe = "\0\1 /\\$%"
    let result = runBufferBuilder $ do
            appendUrlEncoded safe
    assertEqual "matches" "%00%01%20%2f%5c%24%25" result

case_query_current_length :: Assertion
case_query_current_length = do
    let ((f, s), result) = runBufferBuilder' $ do
            appendBS "foo"
            first <- currentLength
            appendBS "bar"
            second <- currentLength
            return (first, second)
    assertEqual "first length" 3 f
    assertEqual "second length" 6 s
    assertEqual "result" "foobar" result

case_calculate_length :: Assertion
case_calculate_length = do
    let first = appendBS "foo"
    let second = first >> appendBS "bar"

    assertEqual "first length" 3 $ calculateLength first
    assertEqual "second length" 6 $ calculateLength second

prop_match_intDec :: Int -> Bool
prop_match_intDec i = runBufferBuilder (appendDecimalSignedInt i) == (BSC.pack $ show i)

instance Arbitrary (BufferBuilder String) where
    arbitrary = oneof
        [ a "appendBS" appendBS
        , a "appendChar8" appendChar8
        , a "appendCharUtf8" appendCharUtf8
        , a "appendStringUtf8" appendStringUtf8
        , a "appendEscapedJson" appendEscapedJson
        , a "appendEscapedJsonText" appendEscapedJsonText
        , a "appendDecimalSignedInt" appendDecimalSignedInt
        , a "appendDecimalDouble" appendDecimalDouble
        , a "appendUrlEncoded" appendUrlEncoded
        ]
      where a :: Arbitrary arg => String -> (arg -> BufferBuilder ()) -> Gen (BufferBuilder String)
            a name action = do
              arg <- arbitrary
              return $ do
                  action arg
                  return name

instance Show (BufferBuilder String) where
    show = fst . runBufferBuilder'

prop_length_calculation_matches_actual_length :: BufferBuilder String -> Bool
prop_length_calculation_matches_actual_length bb =
    let cc = bb >> return () in
    BSC.length (runBufferBuilder cc) == calculateLength cc

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
