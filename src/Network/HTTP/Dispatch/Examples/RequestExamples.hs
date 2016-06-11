{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.RequestExamples where

import           Control.Applicative           ((<$>))
import           Network.HTTP.Dispatch.Core
import           Network.HTTP.Dispatch.Headers as Headers
import           Network.HTTP.Dispatch.Types

-- | Make a simple GET request
--
getRequestExample :: IO HTTPResponse
getRequestExample = runRequest $ get "https://httpbin.org/get"

-- | Get the response code from a request
--
responseCodeExample :: IO Int
responseCodeExample = respStatus <$> getRequestExample

-- | Construct a GET request with added HTTP headers
--
getRequestWithHeadersExample :: HTTPRequest
getRequestWithHeadersExample =
    withHeaders (get "https://httpbin.org/get") [jsonHeader]
      where jsonHeader = header "Content-Type" "application/json"

-- Make a POST request
--
postRequestExample :: HTTPRequest
postRequestExample = post "https://httpbin.org/post" "Hello, World"

-- Basic auth
--
-- @ HTTPRequest { reqMethod = GET,
--                 reqUrl = "http://httpbin.org/get",
--                 reqHeaders = [("Authorization","Basic: dXNlcjpwYXNzd29yZA==")],
--                 reqBody = Nothing
--               }
-- @
requestWithBasicAuthExample :: HTTPRequest
requestWithBasicAuthExample = withHeader (get "http://httpbin.org/get") (Headers.basicAuth "user" "password")
