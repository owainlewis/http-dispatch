{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core
       ( HTTPRequest(..)
       , HTTPResponse(..)
       , HTTPRequestMethod(..)
         -- Core request helpers
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
         -- Extra
       , withQueryParams
       ) where

import qualified Data.ByteString                        as S
import           Network.HTTP.Dispatch.Internal.Request
import           Network.HTTP.Dispatch.Types

import           Data.List                              (intersperse)

-----------------------------------------------------------------------------------
-- Request API
-----------------------------------------------------------------------------------

get :: Url -> HTTPRequest
get url = HTTPRequest GET url [] Nothing

getWithHeaders :: String -> [Header] -> HTTPRequest
getWithHeaders url headers = HTTPRequest GET url headers Nothing

post :: Url -> Body -> HTTPRequest
post url body = postWithHeaders url [] body

postWithHeaders :: Url -> Headers -> Body -> HTTPRequest
postWithHeaders url headers body = HTTPRequest POST url headers (Just body)

put :: Url -> Body -> HTTPRequest
put url body = putWithHeaders url [] body

putWithHeaders :: Url -> Headers -> Body -> HTTPRequest
putWithHeaders url headers body = HTTPRequest PUT url headers (Just body)

patch :: Url -> Body -> HTTPRequest
patch url body = patchWithHeaders url [] body

patchWithHeaders :: Url -> Headers -> Body -> HTTPRequest
patchWithHeaders url headers body = HTTPRequest PATCH url headers (Just body)

delete :: Url -> HTTPRequest
delete url = deleteWithHeaders url []

deleteWithHeaders :: Url -> Headers -> HTTPRequest
deleteWithHeaders url headers = HTTPRequest DELETE url headers Nothing

-- Util

compileParams :: [(String, String)] -> String
compileParams params = "?" ++ kweryParams
     where parts = map (\(k,v) -> mconcat [k, "=", v]) params
           kweryParams = mconcat (intersperse "&" parts)

withQueryParams :: HTTPRequest -> [(String, String)] -> HTTPRequest
withQueryParams req params = req { reqUrl =
                                       let x = reqUrl req
                                           y = compileParams params
                                       in x ++ y
                                 }
