{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell #-}

module BufferTest (tests, main) where

import qualified Data.ByteString.Char8 as BSC
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.BufferBuilder

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

case_query_size :: Assertion
case_query_size = do
    let ((f, s), result) = runBufferBuilder' $ do
            appendBS "foo"
            first <- currentLength
            appendBS "bar"
            second <- currentLength
            return (first, second)
    assertEqual "first size" 3 f
    assertEqual "second size" 6 s
    assertEqual "result" "foobar" result

prop_match_intDec :: Int -> Bool
prop_match_intDec i = runBufferBuilder (appendDecimalSignedInt i) == (BSC.pack $ show i)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
