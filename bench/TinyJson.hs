{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text
import Data.BufferBuilder.Json

data TinyRecord = TR
    { name :: !Text
    , number :: !Int
    }

instance ToJson TinyRecord where
    appendJson !TR{..} = appendJson $
        "name" .= (9::Int)
        -- <> "number" .= number

a :: TinyRecord
a = TR "Bob" 9

main :: IO ()
main = do
    let !b = encodeJson a
    putStrLn $ show b
