{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BufferTest (htf_thisModulesTests) where

import Test.Framework

import qualified Data.ByteString.Char8 as BSC
import Data.BufferBuilder
import Test.QuickCheck.Instances ()

test_append_bytes :: IO ()
test_append_bytes = do
    let result = runBufferBuilder $ do
            appendChar8 'f'
            appendChar8 'o'
            appendChar8 'o'
    assertEqual "fooz" result

test_append_string :: IO ()
test_append_string = do
    let result = runBufferBuilder $ do
            appendChar8 'f'
            appendChar8 'o'
            appendChar8 'o'
            appendBS "bar"
    assertEqual "foobar" result

test_append_literals :: IO ()
test_append_literals = do
    let result = runBufferBuilder $ do
            appendLiteral "foo"#
            appendLiteral "bar"#
    assertEqual "foobar" result

test_append_url_encoded_safe :: IO ()
test_append_url_encoded_safe = do
    let safe = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"
    let result = runBufferBuilder $ do
            appendUrlEncoded safe
    assertEqual safe result

test_append_url_encoded_unsafe :: IO ()
test_append_url_encoded_unsafe = do
    let safe = "\0\1 /\\$%"
    let result = runBufferBuilder $ do
            appendUrlEncoded safe
    assertEqual "%00%01%20%2f%5c%24%25" result

test_query_current_length :: IO ()
test_query_current_length = do
    let ((f, s), result) = runBufferBuilder' $ do
            appendBS "foo"
            first <- currentLength
            appendBS "bar"
            second <- currentLength
            return (first, second)
    assertEqual 3 f
    assertEqual 6 s
    assertEqual "foobar" result

test_calculate_length :: IO ()
test_calculate_length = do
    let first = appendBS "foo"
    let second = first >> appendBS "bar"

    assertEqual 3 $ calculateLength first
    assertEqual 6 $ calculateLength second

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
