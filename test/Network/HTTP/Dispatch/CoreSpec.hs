{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Dispatch.CoreSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (bracket, throwIO)
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.IORef
import Network.HTTP.Client
  ( brRead
  , createCookieJar
  , defaultRequest
  , defaultManagerSettings
  , HttpException (..)
  , HttpExceptionContent (..)
  , managerSetProxy
  , newManager
  , proxyEnvironment
  , useProxy
  )
import Network.HTTP.Dispatch
import Network.HTTP.Types
  ( hContentType
  , hCookie
  , hLocation
  , status200
  , status302
  , status400
  , status503
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  , responseStream
  )
import Network.Wai.Handler.Warp (testWithApplication)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = around withTestServer $ do
  describe "buffered responses" $ do
    it "sends requests with a reusable client" $ \baseUrl -> withFreshClient $ \client -> do
      response <- send client (get (baseUrl <> "/json"))
      responseStatus response `shouldBe` 200
      decoded <- expectRight (decodeJson response)
      responseBody decoded `shouldBe` object ["ok" .= True]

    it "keeps non-2xx responses as values by default" $ \baseUrl -> withFreshClient $ \client -> do
      result <- trySend client (get (baseUrl <> "/unavailable"))
      response <- expectRight result
      responseStatus response `shouldBe` 503

    it "can require a successful status" $ \baseUrl -> withFreshClient $ \client -> do
      result <- trySend client (get (baseUrl <> "/unavailable") & expect2xx)
      case result of
        Left (UnexpectedStatus response) -> responseStatus response `shouldBe` 503
        other -> expectationFailure ("expected UnexpectedStatus, got " <> show other)

    it "enforces a decompressed response size limit" $ \baseUrl -> withFreshClient $ \client -> do
      result <- trySend client (get (baseUrl <> "/large") & maximumResponseBytes 4)
      result `shouldSatisfy` isResponseTooLarge

    it "returns transport timeouts explicitly" $ \baseUrl -> withFreshClient $ \client -> do
      result <- trySend client (get (baseUrl <> "/slow") & withTimeout 1000)
      result `shouldSatisfy` isTransportError

    it "times out a body that stalls after response headers" $ \baseUrl -> withFreshClient $ \client -> do
      result <- trySend client
        (get (baseUrl <> "/stall") & withTimeout 1000000 & withBodyTimeout 1000)
      result `shouldSatisfy` isBodyTimeout

  describe "streaming" $ do
    it "scopes the body reader to the callback" $ \baseUrl -> withFreshClient $ \client -> do
      result <- withStreamingResponse client (get (baseUrl <> "/stream")) $ \response -> do
        first <- brRead (responseBody response)
        second <- brRead (responseBody response)
        pure (first <> second)
      expectRight result `shouldReturn` "abcdef"

    it "supports header-only responses" $ \baseUrl -> withFreshClient $ \client -> do
      result <- sendNoBody client (get (baseUrl <> "/json"))
      response <- expectRight result
      responseBody response `shouldBe` ()

    it "returns streaming body inactivity as an explicit error" $ \baseUrl -> withFreshClient $ \client -> do
      result <- withStreamingResponse client
        (get (baseUrl <> "/stall") & withTimeout 1000000 & withBodyTimeout 1000) $ \response -> do
        _ <- brRead (responseBody response)
        brRead (responseBody response)
      result `shouldSatisfy` isBodyTimeout

    it "never retries after a streaming callback begins" $ \baseUrl -> withFreshClient $ \client -> do
      callbacks <- newIORef (0 :: Int)
      let policy = defaultRetryPolicy
            { retryLimit = 1
            , retryBaseDelayMicros = 0
            , retryMaxDelayMicros = 0
            }
      result <- withStreamingResponse client
        (get (baseUrl <> "/stream") & retrying policy) $ \_ -> do
          modifyIORef' callbacks (+ 1)
          (throwIO (HttpExceptionRequest defaultRequest ConnectionTimeout) :: IO ())
      result `shouldSatisfy` isTransportError
      readIORef callbacks `shouldReturn` 1

  describe "retries" $ do
    it "retries configured transient responses for idempotent methods" $ \_ -> do
      counter <- newIORef 0
      testWithApplication (pure (retryApp counter)) $ \port -> withFreshClient $ \client -> do
        let policy = defaultRetryPolicy
              { retryLimit = 1
              , retryBaseDelayMicros = 0
              , retryMaxDelayMicros = 0
              }
        result <- trySend client
          (get ("http://127.0.0.1:" <> show port)
            & retrying policy
            & maximumResponseBytes 4)
        response <- expectRight result
        responseStatus response `shouldBe` 200
        readIORef counter `shouldReturn` 2

    it "does not retry POST unless the policy explicitly allows it" $ \_ -> do
      counter <- newIORef 0
      testWithApplication (pure (retryApp counter)) $ \port -> withFreshClient $ \client -> do
        let policy = defaultRetryPolicy
              { retryLimit = 1
              , retryBaseDelayMicros = 0
              , retryMaxDelayMicros = 0
              }
        result <- trySend client
          (post ("http://127.0.0.1:" <> show port) & retrying policy)
        response <- expectRight result
        responseStatus response `shouldBe` 503
        readIORef counter `shouldReturn` 1

    it "applies retry policy to header-only sends" $ \_ -> do
      counter <- newIORef 0
      testWithApplication (pure (retryApp counter)) $ \port -> withFreshClient $ \client -> do
        let policy = defaultRetryPolicy
              { retryLimit = 1
              , retryBaseDelayMicros = 0
              , retryMaxDelayMicros = 0
              }
        result <- sendNoBody client
          (get ("http://127.0.0.1:" <> show port) & retrying policy)
        response <- expectRight result
        responseStatus response `shouldBe` 200
        readIORef counter `shouldReturn` 2

    it "applies retry policy before invoking a streaming callback" $ \_ -> do
      counter <- newIORef 0
      testWithApplication (pure (retryApp counter)) $ \port -> withFreshClient $ \client -> do
        callbacks <- newIORef (0 :: Int)
        let policy = defaultRetryPolicy
              { retryLimit = 1
              , retryBaseDelayMicros = 0
              , retryMaxDelayMicros = 0
              }
        result <- withStreamingResponse client
          (get ("http://127.0.0.1:" <> show port) & retrying policy) $ \response -> do
            modifyIORef' callbacks (+ 1)
            pure (responseStatus response)
        expectRight result `shouldReturn` 200
        readIORef counter `shouldReturn` 2
        readIORef callbacks `shouldReturn` 1

    it "reinjects cookies updated by a retryable response" $ \_ -> do
      counter <- newIORef 0
      testWithApplication (pure (retryCookieApp counter)) $ \port ->
        newSessionClient >>= \client -> do
          let policy = defaultRetryPolicy
                { retryLimit = 1
                , retryBaseDelayMicros = 0
                , retryMaxDelayMicros = 0
                }
          result <- trySend client
            (get ("http://127.0.0.1:" <> show port) & retrying policy)
          responseStatus <$> expectRight result `shouldReturn` 200

  describe "sessions" $ do
    it "persists cookies only when a session client is requested" $ \baseUrl ->
      newSessionClient >>= \client -> do
        _ <- send client (get (baseUrl <> "/set-cookie"))
        response <- send client (get (baseUrl <> "/cookie"))
        responseBody response `shouldBe` "token=abc"

    it "does not let an explicit jar contaminate the session" $ \baseUrl ->
      newSessionClient >>= \client -> do
        _ <- send client (get (baseUrl <> "/set-cookie"))
        _ <- send client
          (get (baseUrl <> "/set-explicit") & withCookieJar (createCookieJar []))
        response <- send client (get (baseUrl <> "/cookie"))
        responseBody response `shouldBe` "token=abc"

    it "merges concurrent cookie updates" $ \baseUrl ->
      newSessionClient >>= \client -> do
        first <- newEmptyMVar
        second <- newEmptyMVar
        _ <- forkIO (trySend client (get (baseUrl <> "/set-a")) >>= putMVar first)
        _ <- forkIO (trySend client (get (baseUrl <> "/set-b")) >>= putMVar second)
        firstResult <- takeMVar first
        secondResult <- takeMVar second
        _ <- expectRight firstResult
        _ <- expectRight secondResult
        response <- send client (get (baseUrl <> "/cookie"))
        responseBody response `shouldSatisfy` BS.isInfixOf "a=1"
        responseBody response `shouldSatisfy` BS.isInfixOf "b=2"

    it "strips explicit redirect secrets but applies matching jar cookies" $ \baseUrl -> do
      withFreshClient $ \plainClient -> do
        response <- send plainClient
          (get (baseUrl <> "/redirect")
            & withHeader ("Authorization", "secret-auth")
            & withHeader ("Cookie", "secret-cookie")
            & withHeader ("Proxy-Authorization", "secret-proxy"))
        responseBody response `shouldBe` ""
      newSessionClient >>= \sessionClient -> do
        response <- send sessionClient (get (baseUrl <> "/cookie-redirect"))
        responseBody response `shouldBe` "hop=yes"

  describe "proxies" $ do
    it "disables environment proxies when client options opt out" $ \baseUrl ->
      withEnv "http_proxy" (Just "http://127.0.0.1:1") $
        withEnv "no_proxy" Nothing $ do
          let settings = managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings
              clientOptions = (defaultClientOptions settings)
                { clientProxyPolicy = ProxyFromRequest }
          client <- newClientWith clientOptions
          response <- send client (get baseUrl)
          responseStatus response `shouldBe` 200

    it "preserves a manager proxy selector when requested" $ \baseUrl -> do
      let settings = managerSetProxy (useProxy (Proxy "127.0.0.1" 1)) defaultManagerSettings
          clientOptions = (defaultClientOptions settings)
            { clientProxyPolicy = ProxyFromManager }
      client <- newClientWith clientOptions
      result <- trySend client (get baseUrl & withTimeout 100000)
      result `shouldSatisfy` isTransportError

    it "rejects proxy overrides on a client without an override manager" $ \baseUrl -> do
      manager <- newManager defaultManagerSettings
      result <- trySend (clientFromManager manager) (get baseUrl & withoutProxy)
      case result of
        Left (RequestBuildError _) -> pure ()
        other -> expectationFailure ("expected RequestBuildError, got " <> show other)

    it "routes explicit proxy selections instead of silently going direct" $ \baseUrl ->
      withFreshClient $ \client -> do
        result <- trySend client
          (get baseUrl & withProxy (Proxy "127.0.0.1" 1) & withTimeout 100000)
        result `shouldSatisfy` isTransportError

    it "rejects proxy credentials when a request bypasses its proxy" $ \baseUrl ->
      withFreshClient $ \client -> do
        result <- trySend client
          (get baseUrl & proxyBasicAuth "user" "pass" & withoutProxy)
        case result of
          Left (RequestBuildError _) -> pure ()
          other -> expectationFailure ("expected RequestBuildError, got " <> show other)

    it "sends proxy credentials to an explicitly selected proxy" $ \_ -> do
      captured <- newIORef []
      testWithApplication (pure (proxyApp captured)) $ \port -> withFreshClient $ \client -> do
        response <- send client
          (get "http://example.invalid/resource"
            & withProxy (Proxy "127.0.0.1" port)
            & withHeader ("Proxy-Authorization", "Basic b2xkOm9sZA==")
            & proxyBasicAuth "user" "pass")
        responseBody response `shouldBe` "proxy"
        readIORef captured `shouldReturn` ["Basic dXNlcjpwYXNz"]

    it "preserves proxy policy supplied to the manager compatibility helper" $ \baseUrl ->
      withEnv "http_proxy" (Just "http://127.0.0.1:1") $
        withEnv "no_proxy" Nothing $ do
          response <- httpManager
            (get baseUrl & withoutProxy)
            (managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings)
          responseStatus response `shouldBe` 200

    it "lets a request bypass the client's environment proxy" $ \baseUrl ->
      withEnv "http_proxy" (Just "http://127.0.0.1:1") $
        withEnv "no_proxy" Nothing $
          withFreshClient $ \client -> do
            proxied <- trySend client (get baseUrl & withTimeout 100000)
            proxied `shouldSatisfy` isTransportError
            direct <- trySend client (get baseUrl & withoutProxy)
            responseStatus <$> expectRight direct `shouldReturn` 200

    it "lets the one-shot helper bypass the environment proxy" $ \baseUrl ->
      withEnv "http_proxy" (Just "http://127.0.0.1:1") $
        withEnv "no_proxy" Nothing $ do
          response <- http (get baseUrl & withoutProxy)
          responseStatus response `shouldBe` 200

withFreshClient :: (Client -> IO value) -> IO value
withFreshClient action = newClient >>= action

withTestServer :: ActionWith String -> IO ()
withTestServer action = testWithApplication (pure testApp) $ \port ->
  action ("http://127.0.0.1:" <> show port)

testApp :: Application
testApp requestValue respond = case pathInfo requestValue of
  ["json"] -> respond $ responseLBS status200 [(hContentType, "application/json")] "{\"ok\":true}"
  ["unavailable"] -> respond $ responseLBS status503 [] "later"
  ["large"] -> respond $ responseLBS status200 [] "12345678"
  ["slow"] -> threadDelay 50000 >> respond (responseLBS status200 [] "slow")
  ["stall"] -> respond $ responseStream status200 [] $ \write flush -> do
    write "a"
    flush
    threadDelay 50000
    write "b"
  ["stream"] -> respond $ responseStream status200 [] $ \write flush -> do
    write "abc"
    flush
    write "def"
  ["redirect"] -> respond $ responseLBS status302 [(hLocation, "/redirect-target")] ""
  ["redirect-target"] -> respond $ responseLBS status200 [] . fromStrict . BS.intercalate "|" $
    fmap snd (filter ((`elem` ["Authorization", "Cookie", "Proxy-Authorization"]) . fst)
      (requestHeaders requestValue))
  ["cookie-redirect"] -> respond $ responseLBS status302
    [(hLocation, "/redirect-cookie-target"), ("Set-Cookie", "hop=yes; Path=/")] ""
  ["redirect-cookie-target"] -> respond $ responseLBS status200 [] . fromStrict $
    maybe "" id (lookup hCookie (requestHeaders requestValue))
  ["set-cookie"] -> respond $ responseLBS status200 [("Set-Cookie", "token=abc; Path=/")] "set"
  ["set-explicit"] -> respond $ responseLBS status200 [("Set-Cookie", "explicit=yes; Path=/")] "set"
  ["set-a"] -> respond $ responseLBS status200 [("Set-Cookie", "a=1; Path=/")] "set"
  ["set-b"] -> respond $ responseLBS status200 [("Set-Cookie", "b=2; Path=/")] "set"
  ["cookie"] -> respond $ responseLBS status200 [] . fromStrict $
    maybe "" id (lookup hCookie (requestHeaders requestValue))
  _ -> respond $ responseLBS status200 [] "ok"
  where
    fromStrict = LBS.fromStrict

retryApp :: IORef Int -> Application
retryApp counter _requestValue respond = do
  attempt <- atomicModifyIORef' counter (\value -> let next = value + 1 in (next, next))
  respond $ if attempt == 1
    then responseLBS status503 [] "retry"
    else responseLBS status200 [] "ok"

retryCookieApp :: IORef Int -> Application
retryCookieApp counter requestValue respond = do
  attempt <- atomicModifyIORef' counter (\value -> let next = value + 1 in (next, next))
  if attempt == 1
    then respond $ responseLBS status503 [("Set-Cookie", "gate=yes; Path=/")] "retry"
    else if lookup hCookie (requestHeaders requestValue) == Just "gate=yes"
      then respond $ responseLBS status200 [] "ok"
      else respond $ responseLBS status400 [] "missing retry cookie"

proxyApp :: IORef [BS.ByteString] -> Application
proxyApp captured requestValue respond = do
  writeIORef captured
    [value | (name, value) <- requestHeaders requestValue, name == "Proxy-Authorization"]
  respond $ responseLBS status200 [] "proxy"

isResponseTooLarge :: Either HTTPError response -> Bool
isResponseTooLarge (Left (ResponseTooLarge 4)) = True
isResponseTooLarge _ = False

isTransportError :: Either HTTPError response -> Bool
isTransportError (Left (TransportError _)) = True
isTransportError _ = False

isBodyTimeout :: Either HTTPError response -> Bool
isBodyTimeout (Left BodyTimeout) = True
isBodyTimeout _ = False

expectRight :: Show error => Either error value -> IO value
expectRight (Left err) = expectationFailure (show err) >> fail "expected Right"
expectRight (Right value) = pure value

withEnv :: String -> Maybe String -> IO value -> IO value
withEnv name temporary action = bracket (lookupEnv name) restore (const (set temporary >> action))
  where
    set (Just value) = setEnv name value
    set Nothing = unsetEnv name
    restore = set
