{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core
  ( toRequest
  , runRequest
  , get
  , post
  , postAeson
  , HTTPRequest(..)
  , HTTPRequestMethod
  , HTTPResponse(..)
  ) where

import qualified Data.Aeson                  as Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBC
import           Data.List                   (isPrefixOf)
import           Data.Maybe                  (fromMaybe)
import           Data.String                 (fromString)
import           Network.HTTP.Client         as Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Dispatch.Types (HTTPRequest (..),
                                              HTTPRequestMethod (..),
                                              HTTPResponse (..))
import           Network.HTTP.Types          (RequestHeaders, Status (..))

type Url = String
type Hdrs = [(String, String)]
type Payload = LBS.ByteString

fromResponse :: Response body -> HTTPResponse
fromResponse req =
    let code = statusCode . responseStatus $ req in
    HTTPResponse code

-- Transforms a dispatch request into a low level http-client request
--
toRequest :: HTTPRequest -> IO Client.Request
toRequest (HTTPRequest method url headers body) = do
    initReq <- parseUrl url
    let hdrs = map (\ (k,v) -> (fromString k, fromString v)) (fromMaybe [] headers)
        req = initReq
              { method = C.pack . show $ method
              , requestHeaders = hdrs
              }
    case body of
      Just lbs -> return $ req { requestBody = RequestBodyLBS lbs }
      Nothing -> return $ req

getManagerForUrl :: String -> IO Manager
getManagerForUrl url =
    if ("https" `isPrefixOf` url) then newManager tlsManagerSettings
                                  else newManager defaultManagerSettings

class Runnable a where
  -- Run a HTTP request and return the response
    runRequest :: a -> IO (Response LBS.ByteString)
    -- Run a HTTP request with custom settings (proxy, https etc) and return the response
    runRequestWithSettings :: a -> ManagerSettings -> IO (Response LBS.ByteString)

instance Runnable HTTPRequest where
    runRequest httpRequest = do
        manager <- getManagerForUrl (_url httpRequest)
        request <- toRequest httpRequest
        httpLbs request manager
    runRequestWithSettings httpRequest settings = do
        manager <- newManager settings
        request <- toRequest httpRequest
        httpLbs request manager

-- Request API

get :: Url -> Hdrs -> HTTPRequest
get url headers = HTTPRequest GET url (pure headers) Nothing

post :: Url -> Hdrs -> Payload -> HTTPRequest
post url headers body = HTTPRequest POST url (pure headers) (pure body)

postAeson :: Aeson.ToJSON a => Url -> Hdrs -> a -> HTTPRequest
postAeson url headers body = post url headers (Aeson.encode body)
