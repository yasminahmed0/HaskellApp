{--module Main where

import Lib--}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import ApiRequest
import Lib
import Data.ByteString.Char8 --to handle bytestrings

-- main :: IO ()
-- main = someFunc


main :: IO ()
main = do
  response <- api_call
  let football_data = unpack response
  batchInsert football_data
