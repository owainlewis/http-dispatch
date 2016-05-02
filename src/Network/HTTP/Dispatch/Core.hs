{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core
  ( toRequest
  , runRequest
  , get
  , simpleGet
  , post
  , postString
  , postAeson
  , put
  , patch
  , delete
  ) where

import qualified Data.Aeson                  as Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBSC
import qualified Data.CaseInsensitive        as CI
import           Data.List                   (isPrefixOf)
import           Data.String                 (fromString)
import           Network.HTTP.Client         as Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Dispatch.Types (HTTPRequest (..),
                                              HTTPRequestMethod (..),
                                              HTTPResponse (..), Header (..))
import           Network.HTTP.Types          (RequestHeaders, Status (..))

type Url = String

-- Transforms a dispatch request into a low level http-client request
--
toRequest :: HTTPRequest -> IO Client.Request
toRequest (HTTPRequest method url headers body) = do
    initReq <- parseUrl url
    let hdrs = map (\(k, v) -> (fromString k, fromString v)) headers
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

toResponse :: Client.Response LBS.ByteString -> HTTPResponse
toResponse resp =
    let rStatus = statusCode . responseStatus $ resp
        rHdrs = responseHeaders resp
        rBody = responseBody resp
    in
    HTTPResponse rStatus (map (\(k,v) ->
                                let hk = C.unpack . CI.original $ k
                                    hv = C.unpack v in
                                (hk, hv)) rHdrs) rBody

class Runnable a where
    -- Run a HTTP request and return the response
    runRequest :: a -> IO HTTPResponse
    -- Run a HTTP request with custom settings (proxy, https etc) and return the response
    runRequestWithSettings :: a -> ManagerSettings -> IO HTTPResponse

instance Runnable HTTPRequest where

    runRequest httpRequest = do
        manager <- getManagerForUrl (reqUrl httpRequest)
        request <- toRequest httpRequest
        httpLbs request manager >>= return . toResponse

    runRequestWithSettings httpRequest settings = do
        manager <- newManager settings
        request <- toRequest httpRequest
        httpLbs request manager >>= return . toResponse

-- Request API

get :: Url -> [Header] -> HTTPRequest
get url headers = HTTPRequest GET url headers Nothing

simpleGet :: Url -> HTTPRequest
simpleGet url = get url []

-- Post request with a lazy bytestring payload
post :: Url -> [Header] -> LBS.ByteString -> HTTPRequest
post url headers body = HTTPRequest POST url headers (pure body)

-- Post request with a string payload
postString :: String -> [Header] -> String -> HTTPRequest
postString url headers body = HTTPRequest POST url headers (pure . LBSC.pack $ body)

-- Post request where the payload is some type that has a ToJSON instance defined
postAeson :: Aeson.ToJSON a => Url -> [Header] -> a -> HTTPRequest
postAeson url headers body = post url headers (Aeson.encode body)

put :: String -> [Header] -> LBS.ByteString -> HTTPRequest
put url headers body = HTTPRequest PUT url headers (pure body)

delete :: String -> [Header] -> Maybe LBS.ByteString -> HTTPRequest
delete url headers = HTTPRequest DELETE url headers

patch :: String -> [Header] -> LBS.ByteString -> HTTPRequest
patch url headers body = HTTPRequest PATCH url headers (pure body)
