{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Request
  ( toRequest
  , runRequest
  , compileParams
  , withQueryParams
  ) where

import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.CaseInsensitive        as CI
import           Data.List                   (isPrefixOf)
import           Data.List                   (intersperse)
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
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
    initReq <- parseUrl (C.unpack url)
    let hdrs = map (\(k, v) -> (k, v)) headers
        req = initReq
              { method = C.pack . show $ method
--              , requestHeaders = hdrs
                -- Make sure no exceptions are thrown so that we can handle non 200 codes
              , checkStatus = \_ _ _ -> Nothing
              }
    case body of
      Just lbs ->
        return $ req { requestBody = RequestBodyBS lbs }
      Nothing ->
        return req

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
                                let hk = CI.original $ k
                                    hv = v in
                                (hk, hv)) rHdrs) rBody

compileParams :: [(S.ByteString, S.ByteString)] -> S.ByteString
compileParams params = "?" <> kweryParams
     where parts = map (\(k,v) -> mconcat [k, "=", v]) params
           kweryParams = mconcat $ Data.List.intersperse "&" parts

withQueryParams :: HTTPRequest -> [(S.ByteString, S.ByteString)] -> HTTPRequest
withQueryParams req params = req { reqUrl =
                                       let x = reqUrl req
                                           y = compileParams params
                                       in x <> y
                                 }

class Runnable a where
  -- Run a HTTP request and return the response
    runRequest :: a -> IO HTTPResponse
    -- Run a HTTP request with custom settings (proxy, https etc) and return the response
    runRequestWithSettings :: a -> ManagerSettings -> IO HTTPResponse

instance Runnable HTTPRequest where
    runRequest httpRequest = do
        manager <- getManagerForUrl (C.unpack $ reqUrl httpRequest)
        request <- toRequest httpRequest
        httpLbs request manager >>= return . toResponse

    runRequestWithSettings httpRequest settings = do
        manager <- newManager settings
        request <- toRequest httpRequest
        httpLbs request manager >>= return . toResponse
