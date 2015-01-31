{-# LANGUAGE OverloadedStrings, BangPatterns, RecordWildCards #-}

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
                   , bench "encodeRecord" $ nf encodeType recordValue ]
