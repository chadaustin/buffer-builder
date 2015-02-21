{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Text
import Data.BufferBuilder.Json
import qualified Data.ByteString.Char8 as BSC8

data TinyRecord = TR
    { name :: !Text
    , number :: !Int
    }

instance ToJson TinyRecord where
    toJson !TR{..} = toJson $
        "name"# .=# (9::Int)
        -- <> "number" .= number

a :: TinyRecord
a = TR "Bob" 9

main :: IO ()
main = do
    let !b = encodeJson a
    putStrLn $ BSC8.unpack b
