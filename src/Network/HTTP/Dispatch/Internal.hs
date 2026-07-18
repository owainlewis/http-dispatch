{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Dispatch.Internal
  ( Client (..)
  , ClientOptions (..)
  , ClientProxyPolicy (..)
  , defaultClientOptions
  , HTTPRequest (..)
  , RequestMethod (..)
  , requestMethodBytes
  , RetryPolicy (..)
  , defaultRetryPolicy
  , HTTPResponse (..)
  , mapResponseBody
  , HTTPError (..)
  , StatusPolicy (..)
  , acceptsStatus
  ) where

import Control.Exception (Exception)
import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Int (Int64)
import Data.Text (Text)
import Network.HTTP.Client
  ( CookieJar
  , HttpException
  , Manager
  , ManagerSettings
  , Request
  )
import qualified Network.HTTP.Types as HTTP

-- | Reusable connection pool with optional session cookies.
data Client = Client
  { clientManager :: Manager
  , clientDirectManager :: Maybe Manager
  , clientCookieJar :: Maybe (MVar CookieJar)
  }

-- | Manager, proxy-environment, and cookie-session configuration.
data ClientOptions = ClientOptions
  { managerSettings :: ManagerSettings
  , clientProxyPolicy :: ClientProxyPolicy
  , useCookieJar :: Bool
  }

-- | How a client's primary manager selects proxies.
data ClientProxyPolicy
  = ProxyEnvironment
  | ProxyFromRequest
  | ProxyFromManager
  deriving (Eq, Show)

-- | Use the supplied manager settings with environment proxies and no session.
defaultClientOptions :: ManagerSettings -> ClientOptions
defaultClientOptions settings = ClientOptions
  { managerSettings = settings
  , clientProxyPolicy = ProxyEnvironment
  , useCookieJar = False
  }

-- | Standard HTTP methods plus an extension-method escape hatch.
data RequestMethod
  = HEAD
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | TRACE
  | OPTIONS
  | CONNECT
  | CUSTOM ByteString
  deriving (Eq, Ord, Show)

requestMethodBytes :: RequestMethod -> HTTP.Method
requestMethodBytes HEAD = HTTP.methodHead
requestMethodBytes GET = HTTP.methodGet
requestMethodBytes POST = HTTP.methodPost
requestMethodBytes PUT = HTTP.methodPut
requestMethodBytes PATCH = HTTP.methodPatch
requestMethodBytes DELETE = HTTP.methodDelete
requestMethodBytes TRACE = "TRACE"
requestMethodBytes OPTIONS = HTTP.methodOptions
requestMethodBytes CONNECT = "CONNECT"
requestMethodBytes (CUSTOM value) = value

-- | Bounded retry policy. 'retryLimit' counts attempts after the first.
data RetryPolicy = RetryPolicy
  { retryLimit :: Int
  , retryBaseDelayMicros :: Int
  , retryMaxDelayMicros :: Int
  , retryStatusCodes :: [Int]
  , retryMethods :: [RequestMethod]
  }
  deriving (Eq, Show)

-- | No retries, with safe method and transient-status defaults ready to enable.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { retryLimit = 0
  , retryBaseDelayMicros = 100000
  , retryMaxDelayMicros = 5000000
  , retryStatusCodes = [408, 429, 502, 503, 504]
  , retryMethods = [HEAD, GET, PUT, DELETE, TRACE, OPTIONS]
  }

-- | Determines which received statuses are accepted by send functions.
data StatusPolicy
  = AcceptAnyStatus
  | Require2xx
  | RequireStatus [Int]
  deriving (Eq, Show)

acceptsStatus :: StatusPolicy -> HTTP.Status -> Bool
acceptsStatus AcceptAnyStatus _ = True
acceptsStatus Require2xx status =
  let code = HTTP.statusCode status
  in code >= 200 && code < 300
acceptsStatus (RequireStatus codes) status = HTTP.statusCode status `elem` codes

-- | Opaque request program compiled immediately before it is sent.
data HTTPRequest = HTTPRequest
  { buildHTTPRequest :: IO (Either HTTPError Request)
  , requestRetryPolicy :: RetryPolicy
  , requestReplayable :: Bool
  , requestMaximumResponseBytes :: Maybe Int64
  , requestBodyTimeoutMicros :: Maybe Int
  , requestStatusPolicy :: StatusPolicy
  , requestCookieJarExplicit :: Bool
  , requestOverridesClientProxy :: Bool
  }

-- | Response metadata parameterized by its buffered, decoded, or streaming body.
data HTTPResponse body = HTTPResponse
  { responseStatus :: Int
  , responseStatusMessage :: ByteString
  , responseHeaders :: HTTP.ResponseHeaders
  , responseBody :: body
  , responseCookies :: CookieJar
  , responseHttpVersion :: HTTP.HttpVersion
  }
  deriving (Show)

mapResponseBody :: (a -> b) -> HTTPResponse a -> HTTPResponse b
mapResponseBody f response = response { responseBody = f (responseBody response) }

-- | Failures kept separate from ordinary HTTP status responses.
data HTTPError
  = InvalidUrl String String
  | RequestBuildError Text
  | TransportError HttpException
  | UnexpectedStatus (HTTPResponse ByteString)
  | ResponseTooLarge Int64
  | BodyTimeout
  | DecodeError Text

instance Show HTTPError where
  show (InvalidUrl target reason) =
    "InvalidUrl " <> show target <> " " <> show reason
  show (RequestBuildError reason) = "RequestBuildError " <> show reason
  show (TransportError exception) = "TransportError " <> show exception
  show (UnexpectedStatus response) =
    "UnexpectedStatus {status = " <> show (responseStatus response)
      <> ", bodyBytes = " <> show (ByteString.length (responseBody response)) <> "}"
  show (ResponseTooLarge limit) = "ResponseTooLarge " <> show limit
  show BodyTimeout = "BodyTimeout"
  show (DecodeError reason) = "DecodeError " <> show reason

instance Exception HTTPError
