{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core
       ( HTTPRequest(..)
       , HTTPResponse(..)
       , HTTPRequestMethod(..)
       , runRequest
       , get
       , getWithHeaders
       , post
       , postWithHeaders
       , put
       , putWithHeaders
       , patch
       , patchWithHeaders
       , delete
       , deleteWithHeaders
       , withQueryParams
       ) where

import qualified Data.ByteString                        as S
import           Data.Monoid                            (mconcat)
import           Network.HTTP.Dispatch.Internal.Request
import           Network.HTTP.Dispatch.Types

import           Data.List                              (intersperse)

-- | Make a simple HTTP GET request
--
-- @
-- get "http://google.com"
--
-- HTTPRequest { reqMethod = GET
--             , reqUrl = "http://google.com"
--             , reqHeaders = []
--             , reqBody = Nothing
--             }
-- @
get :: Url -> HTTPRequest
get url = HTTPRequest GET url [] Nothing

-- | Make a simple HTTP GET request with headers
--
-- @
--   getWithHeaders "http://google.com" [header "Content-Type" "application/json"]
--
--   HTTPRequest { reqMethod = GET
--               , reqUrl = "http://google.com"
--               , reqHeaders = [("Content-Type","application/json")]
--               , reqBody = Nothing
--               }
-- @
getWithHeaders :: String -> [Header] -> HTTPRequest
getWithHeaders url headers = HTTPRequest GET url headers Nothing

-- | Make a simple HTTP POST request
--
--
post :: Url -> Body -> HTTPRequest
post url body = postWithHeaders url [] body

-- | Make a HTTP POST request with headers
--
postWithHeaders :: Url -> Headers -> Body -> HTTPRequest
postWithHeaders url headers body = HTTPRequest POST url headers (Just body)

-- | Make a HTTP PUT request
--
put :: Url -> Body -> HTTPRequest
put url body = putWithHeaders url [] body

-- | Make a HTTP PUT request with headers
--
putWithHeaders :: Url -> Headers -> Body -> HTTPRequest
putWithHeaders url headers body = HTTPRequest PUT url headers (Just body)

-- | Make a HTTP PATCH request
--
patch :: Url -> Body -> HTTPRequest
patch url body = patchWithHeaders url [] body

-- | Make a HTTP PATCH request with headers
--
patchWithHeaders :: Url -> Headers -> Body -> HTTPRequest
patchWithHeaders url headers body = HTTPRequest PATCH url headers (Just body)

-- | Make a HTTP DELETE request
--
delete :: Url -> HTTPRequest
delete url = deleteWithHeaders url []

-- | Make a HTTP DELETE request with headers
--
deleteWithHeaders :: Url -> Headers -> HTTPRequest
deleteWithHeaders url headers = HTTPRequest DELETE url headers Nothing

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
