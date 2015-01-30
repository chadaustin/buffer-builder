{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Criterion.Main
import Data.BufferWriter
import Control.Monad

scheme :: ByteString
scheme = "http"

host :: ByteString
host = "example.com"

path :: ByteString
path = "the/path/goes/here"

buildURL :: Int -> ByteString
buildURL times = runBufferWriter $ do
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

main :: IO ()
main = defaultMain [ bench "buildURL" $ whnf buildURL 10 ]
