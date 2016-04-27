module Network.HTTP.Dispatch.Types where

import qualified Data.ByteString.Lazy as LBS

data HTTPMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE deriving ( Eq, Show )

data HTTPRequest = HTTPRequest {
    _method  :: HTTPMethod
  , _url     :: String
  , _headers :: Maybe [(String, String)]
  , _body    :: Maybe LBS.ByteString
} deriving ( Eq, Show )

data HTTPResponse = HTTPResponse {
    status :: Int
} deriving ( Eq, Show )
