{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.HTTP.Dispatch.Request
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP request generation DSL
--
module Network.HTTP.Dispatch.Request
       ( HTTPRequest(..)
       , HTTPResponse(..)
       , HTTPRequestMethod(..)
       , runRequest
       , rawRequest
       , getRequest
       , postRequest
       , putRequest
       , patchRequest
       , deleteRequest
       , headRequest
       , optionsRequest
       , withQueryParams
       ) where

import qualified Data.ByteString                        as S
import           Data.Monoid                            (mconcat)
import           Network.HTTP.Dispatch.Internal.Request
import           Network.HTTP.Dispatch.Types
import           Prelude                                hiding (head)

import           Data.List                              (intersperse)

-- | Given an HTTP request type, run the request and return the response
--
run :: HTTPRequest -> IO HTTPResponse
run req = runRequest req

-- | Make a raw HTTP request
--
-- @
--   raw GET "http://google.com" [header "Content-Type" "application/json"] Nothing
--
--   HTTPRequest { reqMethod = GET
--               , reqUrl = "http://google.com"
--               , reqHeaders = [("Content-Type","application/json")]
--               , reqBody = Nothing
--               }
-- @
rawRequest :: HTTPRequestMethod -> String -> [Header] -> Maybe S.ByteString -> HTTPRequest
rawRequest method url headers body = HTTPRequest method url headers body

-- | Make a simple HTTP GET request with headers
--
-- @
--   getRequest "http://google.com" [header "Content-Type" "application/json"]
--
--   HTTPRequest { reqMethod = GET
--               , reqUrl = "http://google.com"
--               , reqHeaders = [("Content-Type","application/json")]
--               , reqBody = Nothing
--               }
-- @
getRequest :: String -> [Header] -> HTTPRequest
getRequest url headers = rawRequest GET url headers Nothing

-- | Make a HTTP POST request with headers
--
postRequest :: Url -> Headers -> Maybe S.ByteString -> HTTPRequest
postRequest url headers body = rawRequest POST url headers body

-- | Make a HTTP PUT request with headers
--
putRequest :: Url -> Headers -> Maybe S.ByteString -> HTTPRequest
putRequest url headers body = rawRequest PUT url headers body

-- | Make a HTTP PATCH request with headers
--
patchRequest :: Url -> Headers -> Maybe S.ByteString -> HTTPRequest
patchRequest url headers body = HTTPRequest PATCH url headers body

-- | Make a HTTP DELETE request with headers
--
deleteRequest :: Url -> Headers -> HTTPRequest
deleteRequest url headers = rawRequest DELETE url headers Nothing

-- | Make a HTTP OPTIONS request
--
optionsRequest :: Url -> [Header] -> HTTPRequest
optionsRequest url headers = rawRequest OPTIONS url headers Nothing

-- | Make a HTTP HEAD request
--
headRequest :: Url -> [Header] -> HTTPRequest
headRequest url headers = rawRequest HEAD url headers Nothing

-- | Add query params to a request URL
--
-- @
--   withQueryParams (get "http://google.com") [("foo", "bar")]
--
--   HTTPRequest { reqMethod = GET
--               , reqUrl = "http://google.com?foo=bar"
--               , reqHeaders = []
--               , reqBody = Nothing
--               }
-- @
withQueryParams :: HTTPRequest -> [(String, String)] -> HTTPRequest
withQueryParams req params = req { reqUrl = u ++ p }
    where u = reqUrl req
          p = compileParams params
          compileParams params = "?" ++ qParams :: String
            where parts = map (\(k,v) -> mconcat [k, "=", v]) params
                  qParams = mconcat (intersperse "&" parts)
