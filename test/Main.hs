{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} BufferTest
import {-@ HTF_TESTS @-} Utf8Test
import {-@ HTF_TESTS @-} JsonTest

main :: IO ()
main = htfMain htf_importedTests
