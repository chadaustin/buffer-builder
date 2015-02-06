{-# LANGUAGE TemplateHaskell, OverloadedStrings, MagicHash #-}

import Test.Tasty

import qualified BufferTest as BufferTest
import qualified Utf8Test as Utf8Test

allTests :: TestTree
allTests = testGroup "all tests" [ BufferTest.tests, Utf8Test.tests ]

main :: IO ()
main = defaultMain allTests
