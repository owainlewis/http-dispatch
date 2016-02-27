module Network.HTTP.Dispatch.Core where

import           Network.HTTP.Client as Client

data HTTPMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE

data HTTPRequest = HTTPRequest {
    url    :: String
  , method :: HTTPMethod
}

