{-# LANGUAGE TemplateHaskell, OverloadedStrings, MagicHash #-}

import Test.Tasty
import Test.Tasty.TH
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

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
