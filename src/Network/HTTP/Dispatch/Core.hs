{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Core
       ( HTTPRequest(..)
       , HTTPResponse(..)
       , runRequest
       , get
       , post
       , patch
       , delete
       , put
       , simpleGet
       , postString
       , postAeson
       ) where

import qualified Data.Aeson                    as Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBSC
import           Network.HTTP.Dispatch.Request
import           Network.HTTP.Dispatch.Types

type Url = String

-- Request API

get :: Url -> [Header] -> HTTPRequest
get url headers = HTTPRequest GET url headers Nothing

simpleGet :: Url -> HTTPRequest
simpleGet url = HTTPRequest GET url [] Nothing

-- Post request with a lazy bytestring payload
post :: Url -> [Header] -> LBS.ByteString -> HTTPRequest
post url headers body = HTTPRequest POST url headers (pure body)

-- Post request with a string payload
postString :: String -> [Header] -> String -> HTTPRequest
postString url headers body = HTTPRequest POST url headers (pure . LBSC.pack $ body)

-- Post request where the payload is some type that has a ToJSON instance defined
postAeson :: Aeson.ToJSON a => Url -> [Header] -> a -> HTTPRequest
postAeson url headers body = HTTPRequest POST url headers (pure $ Aeson.encode body)

put :: String -> [Header] -> LBS.ByteString -> HTTPRequest
put url headers body = HTTPRequest PUT url headers (pure body)

delete :: String -> [Header] -> Maybe LBS.ByteString -> HTTPRequest
delete url headers = HTTPRequest DELETE url headers

patch :: String -> [Header] -> LBS.ByteString -> HTTPRequest
patch url headers body = HTTPRequest PATCH url headers (pure body)
