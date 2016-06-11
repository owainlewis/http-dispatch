# HTTP Dispatch

> This is the HTTP library I wish I had when first learning Haskell

[![CircleCI](https://circleci.com/gh/owainlewis/http-dispatch.svg?style=svg)](https://circleci.com/gh/owainlewis/http-dispatch)

A high level Haskell HTTP client with a friendly and consistent API.

This library builds upon the http-client library, providing an (IMO) easier and more intuative API

There are only two types (HTTPRequest and HTTPResponse)

## Constructing a request

You can construct HTTP requests manually (method, url, headers body)

```haskell
response :: IO HTTPResponse
response = runRequest (HTTPRequest GET "https://github.com" [] Nothing)
```

Or using a slightly prettier DSL. This DSL does nothing more than create the underlying HTTPRequest type above.

```haskell
-- Build a HTTP request and run it
response :: IO HTTPResponse
response = runRequest (get "https://github.com")
```

The runRequest function just takes a HTTPRequest and turns it into a HTTPResponse

```haskell
runRequest :: HTTPRequest -> IO HTTPResponse
```

### Differences from http-client

* Easier to use (IMO)
* Higher level API
* No exceptions thrown on non 200 status codes
* Supports TLS out of the box
* Headers are just tuples (String, String) so no need to deal with case insensitive byte strings
* Automatic Aeson serialization for POST requests

## Motivation

There are already a couple of really good HTTP clients for Haskell, but typically I'd need to go hunting through documentation just to do even the simplest thing.
This is the HTTP library I wish I had when first learning Haskell.

This library strips back everything to be as simple as possible.
It will transparently support HTTPS and has a very consistent DSL for making requests.

There are only two types. A HTTPRequest and a HTTPResponse.
That's all there is to know about this library.
Some utility functions are provided to make constructing requests easier but it's nothing more than sugar for creating types.

### HTTP Request

You can turn a HTTPRequest into a HTTPResponse by calling "runRequest" on it.

```haskell
data HTTPRequest = HTTPRequest {
   -- A HTTP request method e.g GET POST etc
    reqMethod  :: HTTPRequestMethod
  -- A HTTP request URL
  , reqUrl     :: String
  -- Optional HTTP headers
  , reqHeaders :: [Header]
  -- An optional request body
  , reqBody    :: Maybe S.ByteString
} deriving ( Eq, Show )
```

### HTTP Response

```haskell
data HTTPResponse = HTTPResponse {
    -- The response code
    respStatus  :: Int
    -- The response headers
  , respHeaders :: [Header]
    -- The response body
  , respBody    :: LBS.ByteString
} deriving ( Eq, Show )

```

## Examples

Some examples to help you get started. Remember that everything is just a helper for constructing the HTTPRequest type.

```haskell
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
```
