{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.RequestExamples where

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
responseCodeExample = do
  -- Run the request
  response <- runRequest (get "https://httpbin.org/get")
  -- Get the response code
  let code = respStatus response
  -- Return it
  return $ code

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
requestWithBasicAuthExample = withHeader request (Headers.basicAuth "user" "password")
    where request = get "http://httpbin.org/get"
