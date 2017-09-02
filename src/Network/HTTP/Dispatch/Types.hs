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
