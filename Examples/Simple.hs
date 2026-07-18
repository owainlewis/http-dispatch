{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B8
import Data.Function ((&))
import Network.HTTP.Dispatch

main :: IO ()
main = do
  client <- newClient
  response <- send client $
    get "https://httpbin.org/anything"
      & queryParam "library" "http-dispatch"
      & withHeader ("Accept", "application/json")
      & withTimeout 10000000
      & expect2xx
  B8.putStrLn (responseBody response)
