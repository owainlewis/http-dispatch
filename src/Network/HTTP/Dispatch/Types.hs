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
-- HTTP request type
--
module Network.HTTP.Dispatch.Types
    ( RequestMethod(..)
    , HTTPRequest(..)
    , HTTPResponse(..)
    , Header
    , Headers
    , Url
    , header
    , transformHeaders
    , withHeader
    , withHeaders
    , withBody
    , withMethod
    , get
    , post
    , put
    , delete
    , head
    , options
    ) where

import Prelude hiding (head)

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as SC

type Header  = (S.ByteString, S.ByteString)

type Headers = [Header]

type Url = String

data RequestMethod =
    HEAD
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | TRACE
  | OPTIONS
  | CONNECT deriving ( Eq, Ord, Show )

data HTTPRequest = HTTPRequest {
    method  :: RequestMethod
  , url     :: String
  , headers :: [(S.ByteString, S.ByteString)]
  , body    :: Maybe S.ByteString
} deriving ( Eq, Ord, Show )

data HTTPResponse = HTTPResponse {
    responseStatus  :: Int
  , responseHeaders :: [(S.ByteString, S.ByteString)]
  , resposeBody    :: S.ByteString
} deriving ( Eq, Show )

header :: String -> String -> (S.ByteString, S.ByteString)
header k v = (SC.pack k , SC.pack v)

transformHeaders :: [(String, String)] -> [(S.ByteString, S.ByteString)]
transformHeaders = map (\(k,v) -> header k v)

withHeader :: HTTPRequest -> (S.ByteString, S.ByteString) -> HTTPRequest
withHeader req header = req { headers = header : (headers req) }

withHeaders :: HTTPRequest -> [(S.ByteString, S.ByteString)] -> HTTPRequest
withHeaders req headers = req { headers = headers }

withBody :: HTTPRequest -> S.ByteString -> HTTPRequest
withBody req body = req { body = Just body }

withMethod :: HTTPRequest -> RequestMethod -> HTTPRequest
withMethod req method = req { method = method }

-- | Make a raw HTTP request
--
-- @
--   raw GET "http://google.com" [header "Content-Type" "application/json"] Nothing
--
--   HTTPRequest { method = GET
--               , url = "http://google.com"
--               , headers = [("Content-Type","application/json")]
--               , body = Nothing
--               }
-- @
rawRequest :: RequestMethod -> String -> [Header] -> Maybe S.ByteString -> HTTPRequest
rawRequest method url headers body = HTTPRequest method url headers body

-- | Make a simple HTTP GET request with headers
--
-- @
--   getRequest "http://google.com" [header "Content-Type" "application/json"]
--
--   HTTPRequest { method = GET
--               , url = "http://google.com"
--               , headers = [("Content-Type","application/json")]
--               , body = Nothing
--               }
-- @
get :: String -> [Header] -> HTTPRequest
get url headers = rawRequest GET url headers Nothing

-- | Make a HTTP POST request with headers
--
post :: Url -> Headers -> Maybe S.ByteString -> HTTPRequest
post url headers body = rawRequest POST url headers body

-- | Make a HTTP PUT request with headers
--
put :: Url -> Headers -> Maybe S.ByteString -> HTTPRequest
put url headers body = rawRequest PUT url headers body

-- | Make a HTTP PATCH request with headers
--
patch :: Url -> Headers -> Maybe S.ByteString -> HTTPRequest
patch url headers body = rawRequest PATCH url headers body

-- | Make a HTTP DELETE request with headers
--
delete :: Url -> Headers -> HTTPRequest
delete url headers = rawRequest DELETE url headers Nothing

-- | Make a HTTP OPTIONS request
--
options :: Url -> [Header] -> HTTPRequest
options url headers = rawRequest OPTIONS url headers Nothing

-- | Make a HTTP HEAD request
--
head :: Url -> [Header] -> HTTPRequest
head url headers = rawRequest HEAD url headers Nothing
