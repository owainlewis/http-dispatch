{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.RequestExamples where

import           Network.HTTP.Dispatch.Dispatch
import           Network.HTTP.Dispatch.Headers (basicAuth)
import           Network.HTTP.Dispatch.Request
import           Network.HTTP.Dispatch.Response

import qualified Data.ByteString.Char8 as C

-- | Make a simple GET request
--
getRequestExample :: IO HTTPResponse
getRequestExample = get "https://httpbin.org/get" []

-- | Get the response code from a request
--
responseCodeExample :: IO Int
responseCodeExample = do
  -- Run the request
  response <- get "https://httpbin.org/get" []
  -- Get the response code
  return $ status response
