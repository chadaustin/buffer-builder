{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.BufferBuilder.Json
import qualified Data.ByteString.Char8 as BSC8
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector

main :: IO ()
main = do
    let !v = Vector.fromList [1, 2, 3, 4] :: Vector Int
    let !b = encodeJson v
    putStrLn $ BSC8.unpack b
