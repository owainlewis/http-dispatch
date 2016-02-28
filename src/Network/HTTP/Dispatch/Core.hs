{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core 
  ( HTTPMethod
  , toRequest
  , runRequest
  , get
  , getWithHeaders
  ) where

import           Data.ByteString               as BS
import           Data.ByteString.Char8         as C
import           Data.ByteString.Lazy          as LBS
import           Data.Maybe                    (fromMaybe)
import           Network.HTTP.Client           as Client
import           Network.HTTP.Dispatch.Headers as Dispatch
import           Network.HTTP.Types            (RequestHeaders)

data HTTPMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE deriving ( Eq, Show )

packMethod :: HTTPMethod -> C.ByteString
packMethod = C.pack . show

data HTTPRequest = HTTPRequest {
    _url     :: String
  , _method  :: HTTPMethod
  , _headers :: Maybe RequestHeaders
  , _body    :: Maybe BS.ByteString
} deriving ( Eq, Show )

toRequest :: HTTPRequest -> IO Client.Request
toRequest (HTTPRequest url method headers body) = do
    initReq <- parseUrl url
    let req = initReq {
            method = packMethod method
          , requestHeaders = fromMaybe [] headers
          }
    return req

class Runnable a where
  runRequest :: a -> IO (Response LBS.ByteString)

instance Runnable HTTPRequest where
    runRequest httpRequest = do
        manager <- newManager defaultManagerSettings
        request <- toRequest httpRequest
        httpLbs request manager

get :: String -> HTTPRequest
get url = HTTPRequest url GET Nothing Nothing

getWithHeaders :: String -> RequestHeaders -> HTTPRequest
getWithHeaders url headers = HTTPRequest url GET (Just headers) Nothing

