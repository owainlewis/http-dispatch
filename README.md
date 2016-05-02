# HTTP Dispatch

A high level Haskell HTTP client with a friendly and consistent API.

This library builds upon the http-client library.

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
-- Build a HTTP request
getGithub :: HTTPRequest
getGithub = HTTPRequest GET "https://github.com" [] Nothing

-- Run the request and return a response
response :: IO HTTPResponse
response = runRequest getGithub
```

or you can use the helper functions for a nicer DSL

```haskell
req :: HTTPRequest
req = simpleGet "https://github.com"

response :: IO HTTPResponse
response = runRequest req
```

## Examples

### Simple requests

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Simple where

import           Network.HTTP.Dispatch.Core

-- Making a simple GET request

getReq :: IO HTTPResponse
getReq = runRequest (simpleGet "http://owainlewis.com")

--- Get the status code from a response
status :: IO Int
status = do
  resp <- runRequest $ simpleGet "http://owainlewis.com"
  return (respStatus resp)

-- Making a simple POST request

postReq :: IO HTTPResponse
postReq = runRequest $ postString "http://requestb.in/x8cnvfx8" headers "Hello, World!"
    where headers = [ ("Content-Type", "application/json") ]
```

### Posting Aeson

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Aeson where

import           Data.Aeson
import           Network.HTTP.Dispatch.Core

data User = User { firstName :: String
                 , lastName  :: String
                 , age       :: Int } deriving ( Show )

instance ToJSON User where
    toJSON (User first last age) = object [ "first_name" .= first
                                          , "last_name"  .= last
                                          , "age" .= age
                                          ]

-- Create a POST request with the request body as JSON
request :: HTTPRequest
request = postAeson "http://requestb.in/shxmxvsh" headers (User "Jack" "Dorsey" 30)
    where headers = [("Content-Type", "application/json")]

response :: IO HTTPResponse
response = runRequest request
```
