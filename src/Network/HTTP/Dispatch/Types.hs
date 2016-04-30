module Network.HTTP.Dispatch.Types where

import qualified Data.ByteString.Lazy as LBS

data HTTPRequestMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE deriving ( Eq, Show )

data HTTPRequest = HTTPRequest {
   -- A HTTP request method e.g GET POST etc
    _method  :: HTTPRequestMethod
  -- A HTTP request URL
  , _url     :: String
  -- Optional HTTP headers
  , _headers :: Maybe [(String, String)]
  -- An optional request body
  , _body    :: Maybe LBS.ByteString
} deriving ( Eq, Show )

data HTTPResponse = HTTPResponse {
    status :: Int
} deriving ( Eq, Show )

withHeader :: HTTPRequest -> (String, String) -> HTTPRequest
withHeader req header = req { _headers = fmap (header :) (_headers req) }
