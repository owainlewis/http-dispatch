{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.HTTP.Dispatch.Core
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- A transformation layer between Dispatch types and http client
--
module Network.HTTP.Dispatch.Core
  ( http
  , httpManager
  ) where

import           Control.Applicative            ((<$>))
import qualified Data.ByteString.Char8          as C
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.CaseInsensitive           as CI
import           Data.List                      (isPrefixOf)
import           Network.HTTP.Client            as Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Dispatch.Types    (Header)
import           Network.HTTP.Types             (RequestHeaders, Status (..))

import           Network.HTTP.Dispatch.Types

-- | Transforms a dispatch request into a low level http-client request
--
toRequest :: HTTPRequest -> IO Client.Request
toRequest (HTTPRequest method url headers body) = do
    initReq <- parseRequest url
    let hdrs = map (\(k, v) -> (CI.mk k, v)) headers
        req = initReq
              { Client.method = C.pack . show $ method
              , requestHeaders = hdrs
              }
    return $ maybe req (\lbs -> req { requestBody = RequestBodyBS lbs }) body

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
    let rStatus = statusCode . Client.responseStatus $ resp
        rHdrs = Client.responseHeaders resp
        rBody = LBS.toStrict $ Client.responseBody resp
    in
    HTTPResponse rStatus (map (\(k,v) ->
                                let hk = CI.original k
                                in
                                (hk, v)) rHdrs) rBody

class Runnable a where
    http        :: a -> IO HTTPResponse
    httpManager :: a -> ManagerSettings -> IO HTTPResponse

instance Runnable HTTPRequest where
    http req = do
        manager <- getManagerForUrl (url req)
        request <- toRequest req
        toResponse <$> httpLbs request manager

    httpManager req settings = do
        manager <- newManager settings
        request <- toRequest req
        toResponse <$> httpLbs request manager
