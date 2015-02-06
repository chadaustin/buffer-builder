{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell #-}

module Utf8Test (tests, main) where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Data.BufferBuilder.Utf8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

case_append_string :: Assertion
case_append_string = do
    let str = "foo\NUL\1234"
    let bb = runUtf8Builder $ do
            appendString str
    assertEqual "matches" (Text.pack str) $ TE.decodeUtf8 bb

case_append_chars :: Assertion
case_append_chars = do
    let bb = runUtf8Builder $ do
            appendChar '\NUL'
            appendChar '\x1234'
    assertEqual "matches" (TE.encodeUtf8 "\NUL\x1234") bb

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
