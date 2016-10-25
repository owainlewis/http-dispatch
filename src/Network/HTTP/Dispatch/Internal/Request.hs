{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Internal.Request
  ( toRequest
  , runRequest
  , Runnable
  ) where

import           Control.Applicative         ((<$>))
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.CaseInsensitive        as CI
import           Data.List                   (isPrefixOf)
import           Network.HTTP.Client         as Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Dispatch.Types (HTTPRequest (..),
                                              HTTPRequestMethod (..),
                                              HTTPResponse (..), Header (..))
import           Network.HTTP.Types          (RequestHeaders, Status (..))

-- | Transforms a dispatch request into a low level http-client request
--
toRequest :: HTTPRequest -> IO Client.Request
toRequest (HTTPRequest method url headers body) = do
    initReq <- parseUrl url
    let hdrs = map (\(k, v) -> (CI.mk k, v)) headers
        req = initReq
              { method = C.pack . show $ method
              , requestHeaders = hdrs
                -- Make sure no exceptions are thrown so that we can handle non 200 codes
              , checkStatus = \_ _ _ -> Nothing
              }
    case body of
      Just lbs ->
        return $ req { requestBody = RequestBodyBS lbs }
      Nothing ->
        return req

-- | Get the correct Manager depending on the URL (i.e https vs http)
--
getManagerForUrl :: String -> IO Manager
getManagerForUrl url =
    if ("https" `isPrefixOf` url) then newManager tlsManagerSettings
                                  else newManager defaultManagerSettings

-- | Transforms an http-client response into a dispatch response
--
toResponse :: Client.Response LBS.ByteString -> HTTPResponse
toResponse resp =
    let rStatus = statusCode . responseStatus $ resp
        rHdrs = responseHeaders resp
        rBody = LBS.toStrict $ responseBody resp
    in
    HTTPResponse rStatus (map (\(k,v) ->
                                let hk = CI.original k
                                in
                                (hk, v)) rHdrs) rBody

class Runnable a where
    -- Run a HTTP request and return the response
    runRequest :: a -> IO HTTPResponse
    -- Run a HTTP request with custom settings (proxy, https etc) and return the response
    runRequestWithSettings :: a -> ManagerSettings -> IO HTTPResponse

instance Runnable HTTPRequest where
    runRequest httpRequest = do
        manager <- getManagerForUrl (reqUrl httpRequest)
        request <- toRequest httpRequest
        toResponse <$> httpLbs request manager

    runRequestWithSettings httpRequest settings = do
        manager <- newManager settings
        request <- toRequest httpRequest
        toResponse <$> httpLbs request manager
