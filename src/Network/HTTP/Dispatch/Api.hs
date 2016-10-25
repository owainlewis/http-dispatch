{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Api
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

-- | Constructs a HTTP request from raw components and returns a HTTP response
--
raw
  :: HTTPRequestMethod
  -> String
  -> [Header]
  -> Maybe S.ByteString
  -> IO HTTPResponse
raw method url headers body = runRequest $ Dispatch.rawRequest method url headers body

get
  :: String
  -> [Header]
  -> IO HTTPResponse
get url headers = raw GET url headers Nothing

-- | Send a HTTP POST request
--
post
  :: String
  -> [Header]
  -> Maybe S.ByteString
  -> IO HTTPResponse
post url headers body = raw POST url headers body

-- Send a HTTP POST request
--
put
  :: String
  -> [Header]
  -> Maybe S.ByteString
  -> IO HTTPResponse
put url headers body = raw PUT url headers body

-- | Send a HTTP PATCH request
--
patch
  :: String
  -> [Header]
  -> Maybe S.ByteString
  -> IO HTTPResponse
patch url headers body = raw PATCH url headers body

-- Send a HTTP DELETE request
--
delete
  :: String
  -> [Header]
  -> IO HTTPResponse
delete url headers = raw DELETE url headers Nothing
