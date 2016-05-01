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

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Aeson where

import           Data.Aeson
import           Network.HTTP.Dispatch.Core (HTTPRequest, postAeson, runRequest)

data User = User { firstName :: String
                 , lastName  :: String
                 , age       :: Int } deriving ( Show )

instance ToJSON User where
    toJSON (User first last age) = object [ "first_name" .= first
                                          , "last_name"  .= last
                                          , "age" .= age
                                          ]

-- Create a POST request with the request body as JSON
examplePostRequest :: HTTPRequest
examplePostRequest = postAeson "http://requestb.in/shxmxvsh" [] (User "Jack" "Dorsey" 30)

-- Run the request
-- runRequest examplePostRequest

```
