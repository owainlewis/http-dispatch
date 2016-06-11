# HTTP Dispatch

A high level Haskell HTTP client with a friendly and consistent API.

This library builds upon the http-client library, providing an (IMO) easier and more intuative API

There are only two types (HTTPRequest and HTTPResponse)

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

There are a couple of really good HTTP clients for Haskell but none of them felt particularly intuative.
This library strips back everything to be as simple as possible. It will transparently support HTTPS and has a very
consistent DSL for making requests.

There are only two types. A HTTPRequest and a HTTPResponse. You can turn a HTTPRequest into a HTTPResponse by calling
"runRequest" on it. That's all there is to know about this library. Some utility functions are provided to make
constructing requests easier but it's nothing more than sugar for creating types.

```haskell
data HTTPRequest = HTTPRequest {
   -- A HTTP request method e.g GET POST etc
    reqMethod  :: HTTPRequestMethod
  -- A HTTP request URL
  , reqUrl     :: String
  -- Optional HTTP headers
  , reqHeaders :: [Header]
  -- An optional request body
  , reqBody    :: Maybe LBS.ByteString
} deriving ( Eq, Show )

```

## Constructing a request

You can construct HTTP requests manually (method, url, headers body)

```haskell
response :: IO HTTPResponse
response = runRequest $ HTTPRequest GET "https://github.com" [] Nothing
```

Or using a slightly prettier DSL. This DSL does nothing more than create the underlying HTTPRequest type above.

```
-- Build a HTTP request and run it
response :: IO HTTPResponse
response = runRequest $ get "https://github.com"
```

## Examples

### Simple requests

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Simple where

import           Network.HTTP.Dispatch.Core
import           Network.HTTP.Dispatch.Extra(fromString)

-- Making a simple GET request is as easy as

response :: IO HTTPResponse
response = runRequest (get "http://owainlewis.com")

--- Get the status code from a response
status :: IO Int
status = do
  resp <- runRequest $ get "http://owainlewis.com"
  return (respStatus resp)

-- Making a simple POST request
-- Note that the HTTP body is a Lazy ByteString. The fromString is a helper method to convert for you
postReq :: IO HTTPResponse
postReq = runRequest $ postWithHeaders url headers body
    where url = "http://requestb.in/x8cnvfx8"
          headers = [("Content-Type", "application/json")]
          body = fromString "Hello, World!"
```
