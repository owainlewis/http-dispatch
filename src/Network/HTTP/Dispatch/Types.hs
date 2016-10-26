{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.HTTP.Dispatch.Types where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as SC

type Url = String

type Header = (S.ByteString, S.ByteString)

type Headers = [Header]

type Body = S.ByteString

data HTTPRequestMethod =
    HEAD
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | TRACE
  | OPTIONS
  | CONNECT deriving ( Eq, Show )

data HTTPRequest = HTTPRequest {
   -- A HTTP request method e.g GET POST etc
    reqMethod  :: HTTPRequestMethod
  -- A HTTP request URL
  , reqUrl     :: String
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
  , respBody    :: S.ByteString
} deriving ( Eq, Show )

header :: String -> String -> Header
header k v = (SC.pack k , SC.pack v)

withHeader :: HTTPRequest -> Header -> HTTPRequest
withHeader req header = req { reqHeaders = header : (reqHeaders req) }

withHeaders :: HTTPRequest -> [Header] -> HTTPRequest
withHeaders req headers = req { reqHeaders = headers }

withBody :: HTTPRequest -> S.ByteString -> HTTPRequest
withBody req body = req { reqBody = Just body }

withMethod :: HTTPRequest -> HTTPRequestMethod -> HTTPRequest
withMethod req method = req { reqMethod = method }

dropHeaderWithKey :: HTTPRequest -> S.ByteString -> HTTPRequest
dropHeaderWithKey req@(HTTPRequest _ _ hdrs _) headerKey =
  let filteredHeaders = filter (\(k,v) -> k /= headerKey) hdrs in
      withHeaders req filteredHeaders
