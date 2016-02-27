{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core where

import           Data.ByteString.Lazy as LBS
import           Network.HTTP.Client  as Client

data HTTPMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE

data HTTPRequest = HTTPRequest {
    _url    :: String
  , _method :: HTTPMethod
}

simpleGET url = HTTPRequest url GET

toRequest :: HTTPRequest -> IO Client.Request
toRequest httpRequest = do
    initReq <- parseUrl (_url httpRequest)
    let req = initReq { method = "GET" }
    return req

runRequest :: HTTPRequest -> IO (Response LBS.ByteString)
runRequest httpRequest = do
    manager <- newManager defaultManagerSettings
    request <- toRequest httpRequest
    response <- httpLbs request manager
    return response

