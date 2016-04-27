# HTTP Dispatch

A high level Haskell HTTP client with a friendly and consistent API.

This library builds upon the http-client library. 

There are only two types (HTTPRequest and HTTPResponse)

```haskell
runRequest :: HTTPRequest -> IO HTTPResponse
```

## Constructing a request

You can either construct HTTP requests manually (method, url, headers body)

```haskell
getGithub :: HTTPRequest
getGithub = HTTPRequest GET "https://github.com" Nothing Nothing

response :: IO HTTPResponse
response = runRequest getGithub
```

or you can use the helper functions for a nicer DSL

```haskelll
runRequest $ get "https://github.com"
```

