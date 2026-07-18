{-# LANGUAGE ScopedTypeVariables #-}

-- | Client lifecycle, buffered and streaming execution, and body decoders.
module Network.HTTP.Dispatch.Core
  ( newClient
  , newSessionClient
  , newClientWith
  , clientFromManager
  , clientFromManagers
  , send
  , trySend
  , sendNoBody
  , withStreamingResponse
  , http
  , httpManager
  , decodeJson
  , decodeText
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, catch, throwIO, try)
import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Network.HTTP.Client
  ( BodyReader
  , HttpException
  , Manager
  , ManagerSettings
  , Request
  , Response
  , brRead
  , createCookieJar
  , managerSetProxy
  , newManager
  , proxyEnvironment
  , proxyFromRequest
  )
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (getGlobalManager, tlsManagerSettings)
import Network.HTTP.Types
  ( Header
  , HeaderName
  , Status
  , mkStatus
  , statusCode
  , statusMessage
  )
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Network.HTTP.Dispatch.Internal

-- | Create a reusable HTTP and HTTPS client. Proxy environment variables are
-- honored by default, including @NO_PROXY@.
newClient :: IO Client
newClient = newClientWith (defaultClientOptions tlsManagerSettings)

-- | Create a client with a thread-safe cookie jar shared across requests.
newSessionClient :: IO Client
newSessionClient = newClientWith
  (defaultClientOptions tlsManagerSettings) { useCookieJar = True }

-- | Create a reusable client from explicit options.
newClientWith :: ClientOptions -> IO Client
newClientWith options = do
  let settings = case clientProxyPolicy options of
        ProxyEnvironment ->
          managerSetProxy (proxyEnvironment Nothing) (managerSettings options)
        ProxyFromRequest -> managerSetProxy proxyFromRequest (managerSettings options)
        ProxyFromManager -> managerSettings options
  manager <- newManager settings
  directManager <- case clientProxyPolicy options of
    ProxyFromRequest -> pure (Just manager)
    _ -> Just <$> newManager
      (managerSetProxy proxyFromRequest (managerSettings options))
  jar <- if useCookieJar options
    then Just <$> newMVar (createCookieJar [])
    else pure Nothing
  pure (Client manager directManager jar)

-- | Wrap an existing manager without taking cookie-session ownership.
-- Per-request @withProxy@ and @withoutProxy@ overrides are rejected because an
-- arbitrary manager may replace the request proxy. Use 'clientFromManagers'
-- when overrides are required.
clientFromManager :: Manager -> Client
clientFromManager manager = Client manager Nothing Nothing

-- | Wrap a normal manager and a direct manager. Request proxy overrides use
-- the direct manager, whose proxy policy should be 'proxyFromRequest'.
clientFromManagers :: Manager -> Manager -> Client
clientFromManagers manager directManager = Client manager (Just directManager) Nothing

-- | Send and buffer a response, throwing 'HTTPError' on failure.
send :: Client -> HTTPRequest -> IO (HTTPResponse BS.ByteString)
send client request = trySend client request >>= either throwIO pure

-- | Send and buffer a response with explicit failures.
trySend :: Client -> HTTPRequest -> IO (Either HTTPError (HTTPResponse BS.ByteString))
trySend client dispatchRequest = do
  built <- prepareRequest client dispatchRequest
  case built of
    Left err -> pure (Left err)
    Right clientRequest -> attempt clientRequest 0
  where
    policy = requestRetryPolicy dispatchRequest

    attempt clientRequest attemptNumber = do
      requestForAttempt <- refreshSessionJar client dispatchRequest clientRequest
      result <- performBuffered
        client
        dispatchRequest
        requestForAttempt
        (retryableStatusBeforeBody dispatchRequest clientRequest attemptNumber)
      if shouldRetry dispatchRequest clientRequest attemptNumber result
        then do
          delay <- retryDelayMicros policy attemptNumber result
          when (delay > 0) (threadDelay delay)
          attempt clientRequest (attemptNumber + 1)
        else pure result

-- | Receive response metadata and discard the body.
sendNoBody :: Client -> HTTPRequest -> IO (Either HTTPError (HTTPResponse ()))
sendNoBody client dispatchRequest = do
  built <- prepareRequest client dispatchRequest
  case built of
    Left err -> pure (Left err)
    Right clientRequest -> attempt clientRequest 0
  where
    policy = requestRetryPolicy dispatchRequest
    attempt clientRequest attemptNumber = do
      requestForAttempt <- refreshSessionJar client dispatchRequest clientRequest
      result <- performNoBody client dispatchRequest requestForAttempt
      if shouldRetry dispatchRequest clientRequest attemptNumber result
        then do
          delay <- retryDelayMicros policy attemptNumber result
          when (delay > 0) (threadDelay delay)
          attempt clientRequest (attemptNumber + 1)
        else pure result

-- | Stream a response inside a bracketed callback. The body reader must not
-- escape the callback. The connection is released on success, exception, or
-- early return.
withStreamingResponse
  :: Client
  -> HTTPRequest
  -> (HTTPResponse BodyReader -> IO value)
  -> IO (Either HTTPError value)
withStreamingResponse client dispatchRequest action = do
  built <- prepareRequest client dispatchRequest
  case built of
    Left err -> pure (Left err)
    Right request -> attempt request 0
  where
    policy = requestRetryPolicy dispatchRequest

    attempt request attemptNumber = do
      requestForAttempt <- refreshSessionJar client dispatchRequest request
      result <- try $ Client.withResponse
        requestForAttempt
        (managerForRequest client dispatchRequest) $ \response -> do
          updateSession client dispatchRequest requestForAttempt response
          let metadata = fromClientResponse response { Client.responseBody = () }
          if shouldRetry dispatchRequest request attemptNumber (Right metadata)
            then pure (Left metadata)
            else Right <$> handleResponse response
      case result of
        Left (exception :: HttpException) -> do
          let failure = Left (TransportError exception)
          if shouldRetry dispatchRequest request attemptNumber failure
            then retryAfter request attemptNumber failure
            else pure failure
        Right (Left metadata) -> retryAfter request attemptNumber (Right metadata)
        Right (Right value) -> pure value

    retryAfter request attemptNumber result = do
      delay <- retryDelayMicros policy attemptNumber result
      when (delay > 0) (threadDelay delay)
      attempt request (attemptNumber + 1)

    handleResponse response = do
      let responseReader = timedBodyReader
            (requestBodyTimeoutMicros dispatchRequest)
            (Client.responseBody response)
      if acceptsStatus (requestStatusPolicy dispatchRequest) (Client.responseStatus response)
        then ((Right <$> action
          (fromClientResponse response { Client.responseBody = responseReader }))
            `catch` (pure . Left :: HTTPError -> IO (Either HTTPError value)))
          `catch` (pure . Left . TransportError :: HttpException -> IO (Either HTTPError value))
        else do
          consumed <- consumeBody
            (requestBodyTimeoutMicros dispatchRequest)
            (requestMaximumResponseBytes dispatchRequest)
            (Client.responseBody response)
          pure $ case consumed of
            Left err -> Left err
            Right bytes -> Left . UnexpectedStatus . fromClientResponse $
              response { Client.responseBody = bytes }

-- | One-shot convenience using http-client-tls's process-wide reusable
-- manager. Long-lived applications should prefer an explicit 'Client'.
http :: HTTPRequest -> IO (HTTPResponse BS.ByteString)
http request = do
  manager <- getGlobalManager
  send (Client manager (Just globalDirectManager) Nothing) request

globalDirectManager :: Manager
globalDirectManager = unsafePerformIO $
  newManager (managerSetProxy proxyFromRequest tlsManagerSettings)
{-# NOINLINE globalDirectManager #-}

-- | Compatibility helper for custom manager settings.
httpManager :: HTTPRequest -> ManagerSettings -> IO (HTTPResponse BS.ByteString)
httpManager request settings = do
  manager <- newManager settings
  directManager <- newManager (managerSetProxy proxyFromRequest settings)
  send (clientFromManagers manager directManager) request

-- | Decode a buffered body as JSON while preserving response metadata.
decodeJson :: FromJSON value => HTTPResponse BS.ByteString -> Either HTTPError (HTTPResponse value)
decodeJson response = case eitherDecodeStrict' (responseBody response) of
  Left reason -> Left (DecodeError (Text.pack reason))
  Right value -> Right (mapResponseBody (const value) response)

-- | Decode a buffered body as strict UTF-8 text.
decodeText :: HTTPResponse BS.ByteString -> Either HTTPError (HTTPResponse Text)
decodeText response = case Text.decodeUtf8' (responseBody response) of
  Left reason -> Left (DecodeError (Text.pack (show reason)))
  Right value -> Right (mapResponseBody (const value) response)

performBuffered
  :: Client
  -> HTTPRequest
  -> Request
  -> (Status -> Bool)
  -> IO (Either HTTPError (HTTPResponse BS.ByteString))
performBuffered client dispatchRequest request retryBeforeBody = do
  result <- try $ Client.withResponse request (managerForRequest client dispatchRequest) $ \response -> do
    updateSession client dispatchRequest request response
    if retryBeforeBody (Client.responseStatus response)
      then pure . applyBufferedStatus dispatchRequest . fromClientResponse $
        response { Client.responseBody = BS.empty }
      else do
        consumed <- consumeBody
          (requestBodyTimeoutMicros dispatchRequest)
          (requestMaximumResponseBytes dispatchRequest)
          (Client.responseBody response)
        pure $ case consumed of
          Left err -> Left err
          Right bytes -> applyBufferedStatus dispatchRequest . fromClientResponse $
            response { Client.responseBody = bytes }
  pure $ case result of
    Left (exception :: HttpException) -> Left (TransportError exception)
    Right value -> value

performNoBody
  :: Client
  -> HTTPRequest
  -> Request
  -> IO (Either HTTPError (HTTPResponse ()))
performNoBody client dispatchRequest request = do
  result <- try (Client.httpNoBody request (managerForRequest client dispatchRequest))
  case result of
    Left (exception :: HttpException) -> pure (Left (TransportError exception))
    Right response -> do
      updateSession client dispatchRequest request response
      pure (applyNoBodyStatus dispatchRequest (fromClientResponse response))

consumeBody :: Maybe Int -> Maybe Int64 -> BodyReader -> IO (Either HTTPError BS.ByteString)
consumeBody bodyTimeout limit reader = go 0 []
  where
    go total chunks = do
      chunkResult <- case bodyTimeout of
        Nothing -> Just <$> brRead reader
        Just microseconds -> timeout microseconds (brRead reader)
      case chunkResult of
        Nothing -> pure (Left BodyTimeout)
        Just chunk
          | BS.null chunk -> pure (Right (BS.concat (reverse chunks)))
          | otherwise -> do
              let total' = total + fromIntegral (BS.length chunk)
              case limit of
                Just maximumBytes | total' > maximumBytes ->
                  pure (Left (ResponseTooLarge maximumBytes))
                _ -> go total' (chunk : chunks)

timedBodyReader :: Maybe Int -> BodyReader -> BodyReader
timedBodyReader Nothing reader = reader
timedBodyReader (Just microseconds) reader = do
  result <- timeout microseconds (brRead reader)
  maybe (throwIO BodyTimeout) pure result

fromClientResponse :: Response body -> HTTPResponse body
fromClientResponse response = HTTPResponse
  { responseStatus = statusCode (Client.responseStatus response)
  , responseStatusMessage = statusMessage (Client.responseStatus response)
  , responseHeaders = Client.responseHeaders response
  , responseBody = Client.responseBody response
  , responseCookies = Client.responseCookieJar response
  , responseHttpVersion = Client.responseVersion response
  }

applyBufferedStatus
  :: HTTPRequest
  -> HTTPResponse BS.ByteString
  -> Either HTTPError (HTTPResponse BS.ByteString)
applyBufferedStatus request response
  | statusAccepted request response = Right response
  | otherwise = Left (UnexpectedStatus response)

applyNoBodyStatus
  :: HTTPRequest
  -> HTTPResponse ()
  -> Either HTTPError (HTTPResponse ())
applyNoBodyStatus request response
  | statusAccepted request response = Right response
  | otherwise = Left (UnexpectedStatus (mapResponseBody (const BS.empty) response))

statusAccepted :: HTTPRequest -> HTTPResponse body -> Bool
statusAccepted request response = acceptsStatus
  (requestStatusPolicy request)
  (mkStatus (responseStatus response) (responseStatusMessage response))

buildSafely :: HTTPRequest -> IO (Either HTTPError Request)
buildSafely request =
  buildHTTPRequest request `catch` \(exception :: IOException) ->
    pure (Left (RequestBuildError (Text.pack (show exception))))

prepareRequest :: Client -> HTTPRequest -> IO (Either HTTPError Request)
prepareRequest client request = do
  built <- buildSafely request
  let authenticated = built >>= attachProxyCredentials request
  case (authenticated, clientCookieJar client, clientDirectManager client) of
    (Right _, _, Nothing) | requestOverridesClientProxy request ->
      pure (Left (RequestBuildError
        (Text.pack "Per-request proxy overrides require newClientWith or clientFromManagers")))
    (Right value, Just jarVar, _) | not (requestCookieJarExplicit request) -> do
      jar <- readMVar jarVar
      pure (Right value { Client.cookieJar = Just jar })
    _ -> pure authenticated

attachProxyCredentials :: HTTPRequest -> Request -> Either HTTPError Request
attachProxyCredentials dispatchRequest request = case requestProxyCredentials dispatchRequest of
  Nothing -> Right request
  Just (username, password) -> case Client.proxy request of
    Just _ ->
      let sanitized = request
            { Client.requestHeaders = filter
                ((/= CI.mk (B8.pack "Proxy-Authorization")) . fst)
                (Client.requestHeaders request)
            }
      in Right (Client.applyBasicProxyAuth username password sanitized)
    Nothing -> Left (RequestBuildError (Text.pack
      "proxyBasicAuth requires an explicit withProxy selection"))

updateSession :: Client -> HTTPRequest -> Request -> Response body -> IO ()
updateSession client dispatchRequest request response = case clientCookieJar client of
  Nothing -> pure ()
  Just jarVar
    | requestCookieJarExplicit dispatchRequest -> pure ()
    | otherwise -> case Client.cookieJar request of
        Nothing -> pure ()
        Just snapshot -> modifyMVar_ jarVar $ \latest ->
          pure (mergeCookieChanges snapshot (Client.responseCookieJar response) latest)

refreshSessionJar :: Client -> HTTPRequest -> Request -> IO Request
refreshSessionJar client dispatchRequest request
  | requestCookieJarExplicit dispatchRequest = pure request
  | otherwise = case clientCookieJar client of
      Nothing -> pure request
      Just jarVar -> do
        jar <- readMVar jarVar
        pure request { Client.cookieJar = Just jar }

mergeCookieChanges :: Client.CookieJar -> Client.CookieJar -> Client.CookieJar -> Client.CookieJar
mergeCookieChanges snapshot response latest = Client.createCookieJar
  (changed <> filter (not . superseded) latestCookies)
  where
    snapshotCookies = Client.destroyCookieJar snapshot
    responseCookies = Client.destroyCookieJar response
    latestCookies = Client.destroyCookieJar latest
    changed = filter (not . semanticallyPresent snapshotCookies) responseCookies
    deleted = filter (not . equivalentPresent responseCookies) snapshotCookies
    superseded cookie = equivalentPresent (changed <> deleted) cookie

semanticallyPresent :: [Client.Cookie] -> Client.Cookie -> Bool
semanticallyPresent cookies candidate = any (cookieSemanticEqual candidate) cookies

equivalentPresent :: [Client.Cookie] -> Client.Cookie -> Bool
equivalentPresent cookies candidate = any (Client.equivCookie candidate) cookies

cookieSemanticEqual :: Client.Cookie -> Client.Cookie -> Bool
cookieSemanticEqual left right =
  Client.equivCookie left right
    && Client.cookie_value left == Client.cookie_value right
    && Client.cookie_expiry_time left == Client.cookie_expiry_time right
    && Client.cookie_persistent left == Client.cookie_persistent right
    && Client.cookie_host_only left == Client.cookie_host_only right
    && Client.cookie_secure_only left == Client.cookie_secure_only right
    && Client.cookie_http_only left == Client.cookie_http_only right

managerForRequest :: Client -> HTTPRequest -> Manager
managerForRequest client request
  | requestOverridesClientProxy request =
      maybe (clientManager client) id (clientDirectManager client)
  | otherwise = clientManager client

shouldRetry
  :: HTTPRequest
  -> Request
  -> Int
  -> Either HTTPError (HTTPResponse body)
  -> Bool
shouldRetry request clientRequest attemptNumber result =
  retryableAttempt request clientRequest attemptNumber
    && retryableResult result
  where
    policy = requestRetryPolicy request
    retryableResult (Left (TransportError exception)) = retryableTransport exception
    retryableResult (Left BodyTimeout) = True
    retryableResult (Left (UnexpectedStatus response)) = retryableStatus response
    retryableResult (Right response) = retryableStatus response
    retryableResult _ = False
    retryableStatus response = responseStatus response `elem` retryStatusCodes policy

retryableStatusBeforeBody :: HTTPRequest -> Request -> Int -> Status -> Bool
retryableStatusBeforeBody request clientRequest attemptNumber status =
  retryableAttempt request clientRequest attemptNumber
    && statusCode status `elem` retryStatusCodes (requestRetryPolicy request)

retryableAttempt :: HTTPRequest -> Request -> Int -> Bool
retryableAttempt request clientRequest attemptNumber =
  attemptNumber < retryLimit policy
    && requestReplayable request
    && Client.method clientRequest `elem` fmap requestMethodBytes (retryMethods policy)
  where
    policy = requestRetryPolicy request

retryDelayMicros
  :: RetryPolicy
  -> Int
  -> Either HTTPError (HTTPResponse body)
  -> IO Int
retryDelayMicros policy attemptNumber result = do
  now <- getCurrentTime
  let fromHeader = lookupHeader (CI.mk (B8.pack "Retry-After")) (resultHeaders result)
        >>= parseRetryAfterMicros now
      maximumDelay = max 0 (retryMaxDelayMicros policy)
      exponential = toInteger (retryBaseDelayMicros policy) * (2 ^ min 63 attemptNumber)
      cap = fromInteger (min (toInteger maximumDelay) exponential)
  case fromHeader of
    Just delay -> pure (min maximumDelay delay)
    Nothing -> randomRIO (0, max 0 cap)

resultHeaders :: Either HTTPError (HTTPResponse body) -> [Header]
resultHeaders (Right response) = responseHeaders response
resultHeaders (Left (UnexpectedStatus response)) = responseHeaders response
resultHeaders _ = []

lookupHeader :: HeaderName -> [Header] -> Maybe BS.ByteString
lookupHeader name = fmap snd . find ((== name) . fst)

parseRetryAfterMicros :: UTCTime -> BS.ByteString -> Maybe Int
parseRetryAfterMicros now raw = case B8.readInteger raw of
  Just (seconds, rest) | BS.null rest && seconds >= 0 ->
    Just (boundedInt (seconds * 1000000))
  _ -> do
    timestamp <- parseTimeM True defaultTimeLocale rfc822DateFormat (B8.unpack raw)
    pure (boundedInt (max 0 (floor (diffUTCTime timestamp now * 1000000))))

retryableTransport :: HttpException -> Bool
retryableTransport (Client.HttpExceptionRequest _ content) = case content of
  Client.ResponseTimeout -> True
  Client.ConnectionTimeout -> True
  Client.ConnectionFailure _ -> True
  Client.NoResponseDataReceived -> True
  Client.ResponseBodyTooShort _ _ -> True
  Client.IncompleteHeaders -> True
  _ -> False
retryableTransport (Client.InvalidUrlException _ _) = False

boundedInt :: Integer -> Int
boundedInt value = fromInteger (min (toInteger (maxBound :: Int)) value)
