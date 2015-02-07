{-# LANGUAGE TemplateHaskell, OverloadedStrings, MagicHash #-}

import Test.Tasty

import qualified BufferTest
import qualified Utf8Test
import qualified JsonTest

allTests :: TestTree
allTests = testGroup "all tests"
    [ BufferTest.tests
    , Utf8Test.tests
    , JsonTest.tests
    ]

main :: IO ()
main = defaultMain allTests
