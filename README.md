# HTTP Dispatch

> This is the HTTP library I wish I had when first learning Haskell

[![CircleCI](https://circleci.com/gh/owainlewis/http-dispatch.svg?style=svg)](https://circleci.com/gh/owainlewis/http-dispatch)

Available on Hackage: You'll want to use a version at 0.6.2.0 or greater

https://hackage.haskell.org/package/http-dispatch-0.6.2.0

A high level Haskell HTTP client with a friendly and consistent API.
This library builds upon the http-client library, providing an (IMO) easier and more intuative API

*There are only two types (HTTPRequest and HTTPResponse). Everything else is sugar for constructing these types*

### Differences from http-client

* Simple DSL (only two types HTTPRequest and HTTPResponse)
* Higher level API
* No exceptions thrown on non 200 status codes
* Supports TLS out of the box

### Differences from wreq

* Lighter
* Doesn't require lens package

## Motivation

There are already a couple of really good HTTP clients for Haskell ([Wreq](http://www.serpentine.com/wreq/), [HTTP Client](https://github.com/snoyberg/http-client)), but typically I'd need to go hunting through documentation just to do even the simplest thing (or having to import a different package for https).
This is the HTTP library I wish I had when first learning Haskell.

This library strips back everything to be as simple as possible.
It will transparently support HTTPS and has a very consistent DSL for making requests.

There are only two types. A HTTPRequest and a HTTPResponse. That's all there is to know about this library.

Some utility functions are provided to make constructing requests easier but it's nothing more than sugar for creating types.

### HTTP Request

A HTTP request has a method, url, a list of headers and an optional body. Header is a type synonym for a ByteString pair i.e (S.ByteString, S.ByteString). The body is a strict ByteString.

```haskell
data HTTPRequest = HTTPRequest {
	method  :: RequestMethod
  , url     :: String
  , headers :: [(S.ByteString, S.ByteString)]
  , body    :: Maybe S.ByteString
} deriving ( Eq, Ord, Show )
```

### HTTP Response

A HTTP response has a status, a list of headers, and a response body. Header is a type synonym for a ByteString pair i.e (S.ByteString, S.ByteString). The body is a strict ByteString.

```haskell
data HTTPResponse = HTTPResponse {
	responseStatus  :: Int
  , responseHeaders :: [(S.ByteString, S.ByteString)]
  , resposeBody    :: S.ByteString
} deriving ( Eq, Show )
```

## Examples

Some examples to help you get started.

Remember that *everything* is just sugar for constructing the HTTPRequest type and calling run on it to convert it to a IO HttpResponse.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Example

import qualified          Network.HTTP.Dispatch as Dispatch

response :: IO Dispatch.HttpResponse
response = http request
	where reqeuest = get "http://google.com"

```

If you have any questions comments or feedback let me know <owain@owainlewis.com>
