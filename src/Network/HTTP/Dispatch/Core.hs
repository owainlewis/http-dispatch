{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core
  ( HTTPMethod
  , toRequest
  , runRequest
  , get
  , getWithHeaders
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as LBS
import           Data.Maybe                    (fromMaybe)
import           Network.HTTP.Client           as Client
import qualified Network.HTTP.Dispatch.Headers as Dispatch
import           Network.HTTP.Types            (RequestHeaders)
import qualified Data.Aeson as Aeson
import Network.HTTP.Client.TLS
import Data.String(IsString(..))
import Data.List(isPrefixOf)
import qualified Data.ByteString.Lazy.Char8 as LC

data HTTPMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE deriving ( Eq, Show )

packMethod :: HTTPMethod -> C.ByteString
packMethod = C.pack . show

data HTTPRequest = HTTPRequest {
    _method  :: HTTPMethod
  , _url     :: String
  , _headers :: Maybe RequestHeaders
  , _body    :: Maybe LBS.ByteString
} deriving ( Eq, Show )

data HTTPResponse = HTTPResponse {
    status :: Int
} deriving ( Eq, Show )

toRequest :: HTTPRequest -> IO Client.Request
toRequest (HTTPRequest method url headers body) = do
    initReq <- parseUrl url
    let req = initReq {
      method = packMethod method
    , requestHeaders = fromMaybe [] headers
    }
    return req

class Runnable a where
    runRequest :: a -> IO (Response LBS.ByteString)

-- Automatically provide support for TLS
-- It's likely I'll neeed to expose the underlying settings as well at some point
getManagerForUrl :: (Eq a, Data.String.IsString [a]) => [a] -> IO Manager
getManagerForUrl url =
    if ("https" `isPrefixOf` url) then newManager tlsManagerSettings
                                  else newManager defaultManagerSettings

instance Runnable HTTPRequest where
    runRequest httpRequest = do
        manager <- newManager defaultManagerSettings
        request <- toRequest httpRequest
        httpLbs request manager

-- Request API

get :: String -> HTTPRequest
get url = HTTPRequest GET url Nothing Nothing

getWithHeaders :: String -> RequestHeaders -> HTTPRequest
getWithHeaders url headers = HTTPRequest GET url (Just headers) Nothing

post :: String -> LBS.ByteString -> HTTPRequest
post url body = HTTPRequest POST url Nothing (pure body)

postWithHeaders :: String -> RequestHeaders -> LBS.ByteString -> HTTPRequest
postWithHeaders url headers body = HTTPRequest POST url (pure headers) (pure body)

postAeson :: Aeson.ToJSON a => String -> a -> HTTPRequest
postAeson url body = post url (Aeson.encode body)

postAesonWithHeaders :: Aeson.ToJSON a => String -> RequestHeaders -> a -> HTTPRequest
postAesonWithHeaders url headers body = postWithHeaders url headers (Aeson.encode body)

--- Testing

destination :: String
destination = "http://requestb.in/uu5547uu"
