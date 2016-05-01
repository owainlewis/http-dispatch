module Network.HTTP.Dispatch.Types where

import qualified Data.ByteString.Lazy as LBS

data HTTPRequestMethod =
    GET
  | PUT
  | POST
  | PATCH
  | DELETE deriving ( Eq, Show )

data Header = Header {
    headerName  :: String
  , headerValue :: String
} deriving ( Eq, Show )

headerFromTuple :: (String, String) -> Header
headerFromTuple (k,v) = Header k v

tupleFromHeader :: Header -> (String, String)
tupleFromHeader (Header k v) = (k,v)

data HTTPRequest = HTTPRequest {
   -- A HTTP request method e.g GET POST etc
    reqMethod  :: HTTPRequestMethod
  -- A HTTP request URL
  , reqUrl     :: String
  -- Optional HTTP headers
  , reqHeaders :: [Header]
  -- An optional request body
  , reqBody    :: Maybe LBS.ByteString
} deriving ( Eq, Show )

data HTTPResponse = HTTPResponse {
    -- The response code
    respStatus  :: Int
    -- The response headers
  , respHeaders :: [Header]
    -- The response body
  , respBody    :: LBS.ByteString
} deriving ( Eq, Show )

-- Helper methods
---------------------------------------------------------------------------------------------

withHeader :: HTTPRequest -> Header -> HTTPRequest
withHeader req header = req { reqHeaders = header : (reqHeaders req) }

withHeaders :: HTTPRequest -> [Header] -> HTTPRequest
withHeaders req headers = req { reqHeaders = headers }

dropHeaderWithKey :: HTTPRequest -> String -> HTTPRequest
dropHeaderWithKey req@(HTTPRequest _ _ hdrs _) headerKey =
  let filteredHeaders = filter (\h -> (headerName h) /= headerKey) hdrs in
      withHeaders req filteredHeaders

withBody :: HTTPRequest -> LBS.ByteString -> HTTPRequest
withBody req body = req { reqBody = pure body }

withMethod :: HTTPRequest -> HTTPRequestMethod -> HTTPRequest
withMethod req method = req { reqMethod = method }
