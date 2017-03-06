{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.HTTP.Dispatch.Dispatch
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- A simple Haskell HTTP library
--
-- This module contains the primary user facing DSL for making requests
--
module Network.HTTP.Dispatch.Dispatch
  ( raw
  , get
  , post
  , put
  , patch
  , delete
  ) where

import qualified Data.ByteString                        as S
import qualified Network.HTTP.Dispatch.Request          as Dispatch
import           Network.HTTP.Dispatch.Internal.Request (runRequest)
import           Network.HTTP.Dispatch.Types

import           Network.HTTP.Dispatch.Request
import           Network.HTTP.Dispatch.Response

-- | Constructs a HTTP request from raw components and returns a HTTP response
--
raw
  :: RequestMethod  -- A HTTP request method
  -> String             -- A URL
  -> [(String, String)] -- A list of HTTP headers
  -> Maybe S.ByteString -- An optional HTTP request body
  -> IO HTTPResponse
raw method url headers body =
  let byteStringHeaders = transformHeaders headers in
  runRequest $ HTTPRequest method url byteStringHeaders body

get
  :: String
  -> [(String, String)]
  -> IO HTTPResponse
get url headers = raw GET url headers Nothing

-- | Send a HTTP POST request
--
post
  :: String
  -> [(String, String)]
  -> Maybe S.ByteString
  -> IO HTTPResponse
post url headers body = raw POST url headers body

-- Send a HTTP POST request
--
put
  :: String
  -> [(String, String)]
  -> Maybe S.ByteString
  -> IO HTTPResponse
put url headers body = raw PUT url headers body

-- | Send a HTTP PATCH request
--
patch
  :: String
  -> [(String, String)]
  -> Maybe S.ByteString
  -> IO HTTPResponse
patch url headers body = raw PATCH url headers body

-- Send a HTTP DELETE request
--
delete
  :: String
  -> [(String, String)]
  -> IO HTTPResponse
delete url headers = raw DELETE url headers Nothing
