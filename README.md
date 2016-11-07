# HTTP Dispatch

> This is the HTTP library I wish I had when first learning Haskell

[![CircleCI](https://circleci.com/gh/owainlewis/http-dispatch.svg?style=svg)](https://circleci.com/gh/owainlewis/http-dispatch)

Available on Hackage: You'll want to use a version at 0.6.0.0 or greater

https://hackage.haskell.org/package/http-dispatch-0.6.0.0

A high level Haskell HTTP client with a friendly and consistent API.
This library builds upon the http-client library, providing an (IMO) easier and more intuative API

*There are only two types (HTTPRequest and HTTPResponse). Everything else is sugar for constructing these types*

### Differences from http-client

* Simple DSL (only two types HTTPRequest and HTTPResponse)
* Higher level API
* No exceptions thrown on non 200 status codes
* Supports TLS out of the box
* Headers are just tuples (ByteString, ByteString) so no need to deal with case insensitive byte strings

## Motivation

There are already a couple of really good HTTP clients for Haskell ([Wreq](http://www.serpentine.com/wreq/), [HTTP Client](https://github.com/snoyberg/http-client)), but typically I'd need to go hunting through documentation just to do even the simplest thing.
This is the HTTP library I wish I had when first learning Haskell.

This library strips back everything to be as simple as possible. 
It will transparently support HTTPS and has a very consistent DSL for making requests.

There are only two types. A HTTPRequest and a HTTPResponse. That's all there is to know about this library.

Some utility functions are provided to make constructing requests easier but it's nothing more than sugar for creating types.

### HTTP Request

A HTTP request has a method, url, a list of headers and an optional body. Header is a type synonym for a ByteString pair i.e (S.ByteString, S.ByteString). The body is a strict ByteString. 

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
}
```

### HTTP Response

A HTTP response has a status, a list of headers, and a response body. Header is a type synonym for a ByteString pair i.e (S.ByteString, S.ByteString). The body is a strict ByteString. 

```haskell
data HTTPResponse = HTTPResponse {
    -- The response code
    respStatus  :: Int
    -- The response headers
  , respHeaders :: [Header]
    -- The response body
  , respBody    :: S.ByteString
}

```

## Examples

Some examples to help you get started. 

Remember that *everything* is just sugar for constructing the HTTPRequest type and calling run on it to convert it to a IO HttpResponse.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.RequestExamples where

import           Network.HTTP.Dispatch.Api
import qualified Network.HTTP.Dispatch.Request as Request

-- | Make a simple GET request
--
getExample :: IO HTTPResponse
getExample = get "https://httpbin.org/get" []

-- | Get the response code from a request
--
responseCodeExample :: IO Int
responseCodeExample = do
  -- Run the request
  response <- get "https://httpbin.org/get" []
  -- Get the response code
  return $ respStatus response

```

If you have any questions comments or feedback let me know <owain@owainlewis.com>
