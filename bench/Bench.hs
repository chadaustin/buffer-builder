{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards, MagicHash #-}

import Data.ByteString (ByteString)
import Criterion.Main
import Data.BufferBuilder
import Control.Monad

scheme :: ByteString
scheme = "http"

host :: ByteString
host = "example.com"

path :: ByteString
path = "the/path/goes/here"

buildURL :: Int -> ByteString
buildURL times = runBufferBuilder $ do
    replicateM_ times $ do
        -- Sadly, if you look at the generated code, there are many
        -- continuations (thus indirect jumps, thus inefficient stack
        -- traffic) here.  Even though the test strings are constant,
        -- they become ByteString CAFs, and are tag-checked on every
        -- use.  However, appendBS followed by appendChar8 are folded
        -- into the same generated function.
        appendBS scheme
        appendBS "://"
        appendBS host
        appendChar8 '/'
        appendBS path
        appendChar8 '?'
        appendBS "key"
        appendChar8 '='
        appendBS "value"
        appendChar8 '?'
        appendBS "otherkey"
        appendChar8 '='
        appendBS "othervalue"
        appendChar8 '#'
        appendBS "hashyhashyhashy"

buildURLLiterals :: Int -> ByteString
buildURLLiterals times = runBufferBuilder $ do
    replicateM_ times $ do
        -- literals avoid the CAFs for ByteString constants
        appendLiteral "http"#
        appendLiteral "://"#
        appendLiteral "example.com"#
        appendChar8 '/'
        appendLiteral "the/path/goes/here"#
        appendChar8 '?'
        appendLiteral "key"#
        appendChar8 '='
        appendLiteral "value"#
        appendChar8 '?'
        appendLiteral "otherkey"#
        appendChar8 '='
        appendLiteral "othervalue"#
        appendChar8 '#'
        appendLiteral "hashyhashyhashy"#

data Record = Record
              { f1 :: !ByteString
              , f2 :: !ByteString
              , f3 :: !ByteString
              , f4 :: !ByteString
              , f5 :: !ByteString
              , f6 :: !ByteString
              }

encodeType :: Record -> ByteString
encodeType !(Record{..}) = runBufferBuilder $ do
    -- Because the record elements are strict, all of these appendBS
    -- calls are emitted in one long Cmm/x86 function.  Can't do much
    -- better than that.  :) However, if you look closely, the
    -- BufferWriter handle (accessed through ReaderT) is reloaded from
    -- the stack after all appendBS.  In the future, GHC could realize
    -- it's always the same value and only load it once.
    appendBS f1
    appendBS f2
    appendBS f3
    appendBS f4
    appendBS f5
    appendBS f6

recordValue :: Record
recordValue = Record { f1 = "the wheels on the bus go round and round"
                     , f2 = "round and round, round and round"
                     , f3 = "the seats on the bus go up and down"
                     , f4 = "up and down, up and down"
                     , f5 = "the doors on the bus go open and shut"
                     , f6 = "open and shut, open and shut"
                     }

main :: IO ()
main = defaultMain [ bench "buildURL" $ nf buildURL 10
                   , bench "buildURLLiterals" $ nf buildURLLiterals 10
                   , bench "encodeRecord" $ nf encodeType recordValue ]
