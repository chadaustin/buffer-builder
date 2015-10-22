{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell #-}

module Utf8Test (htf_thisModulesTests) where

import Test.Framework

import Data.BufferBuilder.Utf8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

test_append_string :: IO ()
test_append_string = do
    let str = "foo\NUL\1234"
    let bb = runUtf8Builder $ do
            appendString str
    assertEqual (Text.pack str) $ TE.decodeUtf8 bb

test_append_chars :: IO ()
test_append_chars = do
    let bb = runUtf8Builder $ do
            appendChar '\NUL'
            appendChar '\x1234'
    assertEqual (TE.encodeUtf8 "\NUL\x1234") bb
