{-# LANGUAGE TemplateHaskell, OverloadedStrings, MagicHash #-}

import Test.Tasty

import qualified BufferTest as BufferTest

allTests :: TestTree
allTests = testGroup "all tests" [ BufferTest.tests ]

main :: IO ()
main = defaultMain allTests
