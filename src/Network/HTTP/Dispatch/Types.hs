{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.HTTP.Dispatch.Types where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as LBS

data HTTPRequestMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE
  deriving ( Eq, Show )

type Header = (S.ByteString, S.ByteString)

data HTTPRequest = HTTPRequest {
   -- A HTTP request method e.g GET POST etc
    reqMethod  :: HTTPRequestMethod
  -- A HTTP request URL
  , reqUrl     :: S.ByteString
  -- Optional HTTP headers
  , reqHeaders :: [Header]
  -- An optional request body
  , reqBody    :: Maybe S.ByteString
} deriving ( Eq, Show )

data HTTPResponse = HTTPResponse {
    -- The response code
    respStatus  :: Int
    -- The response headers
  , respHeaders :: [Header]
    -- The response body
  , respBody    :: LBS.ByteString
} deriving ( Eq, Show )

-- Update requests (additive)
---------------------------------------------------------------------------------------------

withHeader :: HTTPRequest -> Header -> HTTPRequest
withHeader req header = req { reqHeaders = header : (reqHeaders req) }

withHeaders :: HTTPRequest -> [Header] -> HTTPRequest
withHeaders req headers = req { reqHeaders = headers }

withBody :: HTTPRequest -> S.ByteString -> HTTPRequest
withBody req body = req { reqBody = pure body }

withMethod :: HTTPRequest -> HTTPRequestMethod -> HTTPRequest
withMethod req method = req { reqMethod = method }

---------------------------------------------------------------------------------------------

dropHeaderWithKey :: HTTPRequest -> S.ByteString -> HTTPRequest
dropHeaderWithKey req@(HTTPRequest _ _ hdrs _) headerKey =
  let filteredHeaders = filter (\(k,v) -> k /= headerKey) hdrs in
      withHeaders req filteredHeaders
