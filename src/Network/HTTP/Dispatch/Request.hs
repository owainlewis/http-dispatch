{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pipeable request constructors and transformations. Functions take the
-- value being added first and the request last, so they compose with @(&)@.
module Network.HTTP.Dispatch.Request
  ( request
  , raw
  , get
  , head
  , post
  , put
  , patch
  , delete
  , options
  , customMethod
  , header
  , transformHeaders
  , withHeader
  , setHeader
  , withHeaders
  , setHeaders
  , queryParam
  , queryParamText
  , queryFlag
  , pathSegment
  , withBody
  , withLazyBody
  , withRequestBody
  , withJsonBody
  , withFormBody
  , withMultipartBody
  , withMethod
  , basicAuth
  , bearerAuth
  , proxyBasicAuth
  , withProxy
  , withoutProxy
  , withTimeout
  , withoutTimeout
  , withRedirects
  , withoutRedirects
  , withCookieJar
  , rawResponseBody
  , maximumResponseBytes
  , unlimitedResponseBody
  , acceptAnyStatus
  , expect2xx
  , expectStatus
  , retrying
  , retryable
  , modifyClientRequest
  , inspectRequest
  , partBS
  , partLBS
  , partFile
  , partFileSource
  ) where

import Prelude hiding (head)

import Control.Exception (try)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Data.CaseInsensitive (mk)
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client
  ( CookieJar
  , Proxy
  , Request
  , RequestBody (..)
  , applyBasicAuth
  , applyBearerAuth
  , applyBasicProxyAuth
  , parseRequest
  , responseTimeoutMicro
  , responseTimeoutNone
  , setQueryString
  )
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.MultipartFormData
  ( Part
  , formDataBody
  , partBS
  , partFile
  , partFileSource
  , partLBS
  )
import Network.HTTP.Types
  ( Header
  , HeaderName
  , QueryItem
  , hAuthorization
  , hContentType
  , renderSimpleQuery
  )
import Network.HTTP.Types.URI (encodePathSegments, parseQuery)

import Network.HTTP.Dispatch.Internal
import Network.HTTP.Dispatch.Types (Headers, Url)

-- | Build a request for any supported method. Invalid URLs become 'HTTPError'.
request :: RequestMethod -> Url -> HTTPRequest
request requestMethod target = HTTPRequest
  { buildHTTPRequest = do
      if validRequestMethod requestMethod
        then do
          parsed <- try (parseRequest target)
          pure $ case parsed of
            Left (Client.InvalidUrlException badUrl reason) -> Left (InvalidUrl badUrl reason)
            Left (exception :: Client.HttpException) -> Left (TransportError exception)
            Right base -> Right base
              { Client.method = requestMethodBytes requestMethod
              , Client.checkResponse = \_ _ -> pure ()
              , Client.shouldStripHeaderOnRedirect = sensitiveHeader
              , Client.shouldStripHeaderOnRedirectIfOnDifferentHostOnly = False
              , Client.redactHeaders = Set.fromList
                  [ hAuthorization, "Cookie", "Proxy-Authorization", "Set-Cookie" ]
              }
        else pure (Left (RequestBuildError (Text.pack "Invalid HTTP method")))
  , requestRetryPolicy = defaultRetryPolicy
  , requestReplayable = True
  , requestMaximumResponseBytes = Just (16 * 1024 * 1024)
  , requestBodyTimeoutMicros = Just 30000000
  , requestStatusPolicy = AcceptAnyStatus
  , requestCookieJarExplicit = False
  , requestOverridesClientProxy = False
  }

-- | Compatibility constructor for a method, URL, headers, and strict body.
raw :: RequestMethod -> Url -> Headers -> Maybe BS.ByteString -> HTTPRequest
raw requestMethod target requestHeaders requestBody =
  maybe id withBody requestBody
    $ setHeaders requestHeaders
    $ request requestMethod target

-- | Standard verb constructors.
get, head, post, put, patch, delete, options :: Url -> HTTPRequest
get = request GET
head = request HEAD
post = request POST
put = request PUT
patch = request PATCH
delete = request DELETE
options = request OPTIONS

-- | Construct a request using an extension method such as @PURGE@.
customMethod :: BS.ByteString -> Url -> HTTPRequest
customMethod value = request (CUSTOM value)

-- | Construct an ASCII-compatible header from strings.
header :: String -> String -> Header
header name value = (mk (B8.pack name), B8.pack value)

-- | Convert string pairs to ordered headers.
transformHeaders :: [(String, String)] -> Headers
transformHeaders = fmap (uncurry header)

-- | Append one header without removing duplicates.
withHeader :: Header -> HTTPRequest -> HTTPRequest
withHeader value = mapClientRequest $ \req ->
  req { Client.requestHeaders = Client.requestHeaders req <> [value] }

-- | Replace every header with the same case-insensitive name.
setHeader :: Header -> HTTPRequest -> HTTPRequest
setHeader value@(name, _) = mapClientRequest $ \req -> req
  { Client.requestHeaders =
      filter ((/= name) . fst) (Client.requestHeaders req) <> [value]
  }

-- | Append headers without removing duplicates.
withHeaders :: Headers -> HTTPRequest -> HTTPRequest
withHeaders values = mapClientRequest $ \req ->
  req { Client.requestHeaders = Client.requestHeaders req <> values }

-- | Replace all headers.
setHeaders :: Headers -> HTTPRequest -> HTTPRequest
setHeaders values = mapClientRequest $ \req -> req { Client.requestHeaders = values }

-- | Append a percent-encoded query parameter, preserving order and duplicates.
queryParam :: BS.ByteString -> BS.ByteString -> HTTPRequest -> HTTPRequest
queryParam key value = addQueryItem (key, Just value)

-- | UTF-8 text variant of 'queryParam'.
queryParamText :: Text -> Text -> HTTPRequest -> HTTPRequest
queryParamText key value = queryParam (Text.encodeUtf8 key) (Text.encodeUtf8 value)

-- | Append a key without an equals sign.
queryFlag :: BS.ByteString -> HTTPRequest -> HTTPRequest
queryFlag key = addQueryItem (key, Nothing)

-- | Append one UTF-8 path segment, percent-encoding it exactly once.
pathSegment :: Text -> HTTPRequest -> HTTPRequest
pathSegment segment = mapClientRequest $ \req ->
  let encoded = BS.drop 1 . LBS.toStrict . Builder.toLazyByteString
        $ encodePathSegments [segment]
      separator
        | BS.null (Client.path req) = "/"
        | BS.last (Client.path req) == 47 = ""
        | otherwise = "/"
  in req { Client.path = Client.path req <> separator <> encoded }

-- | Set a replayable strict body.
withBody :: BS.ByteString -> HTTPRequest -> HTTPRequest
withBody value = setBody True (RequestBodyBS value)

-- | Set a replayable lazy body.
withLazyBody :: LBS.ByteString -> HTTPRequest -> HTTPRequest
withLazyBody value = setBody True (RequestBodyLBS value)

-- | Set an arbitrary backend body and declare whether it is replayable.
withRequestBody :: Bool -> RequestBody -> HTTPRequest -> HTTPRequest
withRequestBody = setBody

-- | Encode JSON and append its content type.
withJsonBody :: ToJSON value => value -> HTTPRequest -> HTTPRequest
withJsonBody value =
  setHeader (hContentType, "application/json") . withLazyBody (encode value)

-- | Encode an @application/x-www-form-urlencoded@ body.
withFormBody :: [(BS.ByteString, BS.ByteString)] -> HTTPRequest -> HTTPRequest
withFormBody values =
  setHeader (hContentType, "application/x-www-form-urlencoded")
    . setBody True (RequestBodyBS (renderSimpleQuery False values))

-- | Build a multipart body. File-source parts stream rather than buffer.
withMultipartBody :: [Part] -> HTTPRequest -> HTTPRequest
withMultipartBody parts req = req
  { buildHTTPRequest = do
      built <- buildHTTPRequest req
      traverse (\base -> do
        formed <- formDataBody parts base
        pure formed { Client.method = Client.method base }) built
  , requestReplayable = requestReplayable req
  }

-- | Replace the method.
withMethod :: RequestMethod -> HTTPRequest -> HTTPRequest
withMethod requestMethod = mapClientRequest $ \req ->
  req { Client.method = requestMethodBytes requestMethod }

-- | Apply HTTP Basic authorization.
basicAuth :: BS.ByteString -> BS.ByteString -> HTTPRequest -> HTTPRequest
basicAuth username password = mapClientRequest $ \req ->
  applyBasicAuth username password (removeHeader hAuthorization req)

-- | Apply bearer-token authorization.
bearerAuth :: BS.ByteString -> HTTPRequest -> HTTPRequest
bearerAuth token = mapClientRequest $ \req ->
  applyBearerAuth token (removeHeader hAuthorization req)

-- | Apply Basic credentials for the selected proxy.
proxyBasicAuth :: BS.ByteString -> BS.ByteString -> HTTPRequest -> HTTPRequest
proxyBasicAuth username password = mapClientRequest $ \req ->
  applyBasicProxyAuth username password (removeHeader "Proxy-Authorization" req)

-- | Select an explicit proxy for one request.
withProxy :: Proxy -> HTTPRequest -> HTTPRequest
withProxy proxy requestValue =
  (mapClientRequest (\req -> req { Client.proxy = Just proxy }) requestValue)
    { requestOverridesClientProxy = True }

-- | Disable both the request proxy and the client's environment proxy.
withoutProxy :: HTTPRequest -> HTTPRequest
withoutProxy requestValue =
  (mapClientRequest (\req -> req { Client.proxy = Nothing }) requestValue)
    { requestOverridesClientProxy = True }

-- | Set connection/header and per-body-read inactivity timeouts in microseconds.
withTimeout :: Int -> HTTPRequest -> HTTPRequest
withTimeout microseconds requestValue =
  (mapClientRequest (\req ->
    req { Client.responseTimeout = responseTimeoutMicro (max 0 microseconds) }) requestValue)
    { requestBodyTimeoutMicros = Just (max 0 microseconds) }

-- | Disable connection, header, and body inactivity timeouts.
withoutTimeout :: HTTPRequest -> HTTPRequest
withoutTimeout requestValue =
  (mapClientRequest (\req -> req { Client.responseTimeout = responseTimeoutNone }) requestValue)
    { requestBodyTimeoutMicros = Nothing }

-- | Set the maximum redirect count.
withRedirects :: Int -> HTTPRequest -> HTTPRequest
withRedirects count = mapClientRequest $ \req -> req { Client.redirectCount = max 0 count }

-- | Refuse redirects.
withoutRedirects :: HTTPRequest -> HTTPRequest
withoutRedirects = withRedirects 0

-- | Use an explicit cookie jar, overriding a session jar for this request.
withCookieJar :: CookieJar -> HTTPRequest -> HTTPRequest
withCookieJar jar requestValue =
  (mapClientRequest (\req -> req { Client.cookieJar = Just jar }) requestValue)
    { requestCookieJarExplicit = True }

-- | Disable automatic decompression.
rawResponseBody :: HTTPRequest -> HTTPRequest
rawResponseBody = mapClientRequest $ \req -> req { Client.decompress = const False }

-- | Bound buffered bytes after decompression.
maximumResponseBytes :: Int64 -> HTTPRequest -> HTTPRequest
maximumResponseBytes bytes req = req { requestMaximumResponseBytes = Just (max 0 bytes) }

-- | Remove the buffered body bound. Prefer streaming for untrusted responses.
unlimitedResponseBody :: HTTPRequest -> HTTPRequest
unlimitedResponseBody req = req { requestMaximumResponseBytes = Nothing }

-- | Treat every received status as a response value.
acceptAnyStatus :: HTTPRequest -> HTTPRequest
acceptAnyStatus req = req { requestStatusPolicy = AcceptAnyStatus }

-- | Turn non-2xx statuses into 'UnexpectedStatus'.
expect2xx :: HTTPRequest -> HTTPRequest
expect2xx req = req { requestStatusPolicy = Require2xx }

-- | Accept only the listed status codes.
expectStatus :: [Int] -> HTTPRequest -> HTTPRequest
expectStatus codes req = req { requestStatusPolicy = RequireStatus codes }

-- | Enable the supplied bounded retry policy.
retrying :: RetryPolicy -> HTTPRequest -> HTTPRequest
retrying policy req = req { requestRetryPolicy = sanitizeRetryPolicy policy }

-- | Explicitly mark the current body as safe to recreate for a retry.
retryable :: HTTPRequest -> HTTPRequest
retryable req = req { requestReplayable = True }

-- | Advanced escape hatch applied after all ordinary request construction.
modifyClientRequest :: (Request -> IO Request) -> HTTPRequest -> HTTPRequest
modifyClientRequest f req = req
  { buildHTTPRequest = buildHTTPRequest req >>= traverse f
  }

-- | Compile without sending. Useful for tests and custom backends.
inspectRequest :: HTTPRequest -> IO (Either HTTPError Request)
inspectRequest = buildHTTPRequest

mapClientRequest :: (Request -> Request) -> HTTPRequest -> HTTPRequest
mapClientRequest f = modifyClientRequest (pure . f)

removeHeader :: HeaderName -> Request -> Request
removeHeader name req = req
  { Client.requestHeaders = filter ((/= name) . fst) (Client.requestHeaders req) }

setBody :: Bool -> RequestBody -> HTTPRequest -> HTTPRequest
setBody replayable body req =
  (mapClientRequest (\value -> value { Client.requestBody = body }) req)
    { requestReplayable = replayable }

addQueryItem :: QueryItem -> HTTPRequest -> HTTPRequest
addQueryItem item = mapClientRequest $ \req ->
  setQueryString (parseQuery (Client.queryString req) <> [item]) req

sensitiveHeader :: HeaderName -> Bool
sensitiveHeader name = name `elem` [hAuthorization, "Cookie", "Proxy-Authorization", "Host"]

sanitizeRetryPolicy :: RetryPolicy -> RetryPolicy
sanitizeRetryPolicy policy = policy
  { retryLimit = max 0 (retryLimit policy)
  , retryBaseDelayMicros = max 0 (retryBaseDelayMicros policy)
  , retryMaxDelayMicros = max 0 (retryMaxDelayMicros policy)
  }

validRequestMethod :: RequestMethod -> Bool
validRequestMethod (CUSTOM value) = not (BS.null value) && BS.all isTokenChar value
validRequestMethod _ = True

isTokenChar :: Word8 -> Bool
isTokenChar value =
  (value >= 48 && value <= 57)
    || (value >= 65 && value <= 90)
    || (value >= 97 && value <= 122)
    || value `elem` fmap (fromIntegral . fromEnum) ("!#$%&'*+-.^_`|~" :: String)
