module Network.HTTP.Dispatch.Request
  ( raw
  , get
  , post
  , put
  , patch
  , delete
  , options
  ) where

import Network.HTTP.Dispatch.Types

import qualified Data.ByteString       as S

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
raw :: RequestMethod -> String -> [Header] -> Maybe S.ByteString -> HTTPRequest
raw method url headers body = HTTPRequest method url headers body

get :: String -> HTTPRequest
get url = raw GET url [] Nothing

post :: Url -> Maybe S.ByteString -> HTTPRequest
post url body = raw POST url [] body

put :: Url -> Maybe S.ByteString -> HTTPRequest
put url body = raw PUT url [] body

patch :: Url -> Maybe S.ByteString -> HTTPRequest
patch url body = raw PATCH url [] body

delete :: Url -> HTTPRequest
delete url = raw DELETE url [] Nothing

options :: Url -> HTTPRequest
options url = raw OPTIONS url [] Nothing
