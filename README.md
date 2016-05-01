# HTTP Dispatch

A high level Haskell HTTP client with a friendly and consistent API.

This library builds upon the http-client library.

There are only two types (HTTPRequest and HTTPResponse)

```haskell
runRequest :: HTTPRequest -> IO HTTPResponse
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
runRequest $ simpleGet "https://github.com"
```

## Examples

### Simple requests

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Simple where

import           Control.Applicative         ((<$>))
import           Network.HTTP.Dispatch.Core
import           Network.HTTP.Dispatch.Types

-- Making a simple GET request

getReq :: IO HTTPResponse
getReq = runRequest (simpleGet "http://owainlewis.com")

--- Get the status code from a response
status :: IO Int
status = do
  resp <- runRequest $ simpleGet "http://owainlewis.com"
  return (respStatus resp)

-- Making a simple POST request

postReq = runRequest $ postString "http://requestb.in/x8cnvfx8" headers "Hello, World!"
    where headers = [ ("Content-Type", "application/json") ]
```

### Posting Aeson

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Aeson where

import           Data.Aeson
import           Network.HTTP.Dispatch.Core  (postAeson, runRequest)
import           Network.HTTP.Dispatch.Types

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
request = postAeson "http://requestb.in/shxmxvsh" headers body
    where headers = [("Content-Type", "application/json")]
          body = User "Jack" "Dorsey" 30

response :: IO HTTPResponse
response = runRequest request
```
