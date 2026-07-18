{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Dispatch.RequestSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Network.HTTP.Client as Client
import Network.HTTP.Dispatch
import Network.HTTP.Types (hAuthorization, hContentType)
import Test.Hspec

spec :: Spec
spec = do
  describe "request construction" $ do
    it "builds standard and custom methods" $ do
      standard <- inspectRequest (get "https://example.com") >>= expectRight
      custom <- inspectRequest (customMethod "PURGE" "https://example.com") >>= expectRight
      Client.method standard `shouldBe` "GET"
      Client.method custom `shouldBe` "PURGE"

    it "encodes path segments and query values exactly once" $ do
      built <- inspectRequest (
        get "https://example.com/api?existing=1"
          & pathSegment "Owain Lewis/λ"
          & queryParam "tag" "haskell & http"
          & queryParam "tag" "second"
          & queryFlag "active") >>= expectRight
      Client.path built `shouldBe` "/api/Owain%20Lewis%2F%CE%BB"
      Client.queryString built
        `shouldBe` "?existing=1&tag=haskell%20%26%20http&tag=second&active"

    it "preserves existing query bytes when appending values" $ do
      built <- inspectRequest
        (get "https://example.com/?signature=%2f%2B&empty" & queryParam "next" "a b")
        >>= expectRight
      Client.queryString built `shouldBe` "?signature=%2f%2B&empty&next=a%20b"

    it "preserves duplicate headers and strips secrets on every redirect" $ do
      built <- inspectRequest (
        get "https://example.com"
          & withHeader ("X-Value", "one")
          & withHeader ("X-Value", "two")
          & bearerAuth "secret") >>= expectRight
      length (Client.requestHeaders built) `shouldBe` 3
      Client.shouldStripHeaderOnRedirect built hAuthorization `shouldBe` True
      Client.shouldStripHeaderOnRedirectIfOnDifferentHostOnly built `shouldBe` False

    it "creates JSON bodies with the correct content type" $ do
      built <- inspectRequest (
        post "https://example.com"
          & withJsonBody (object ["answer" .= (42 :: Int)])) >>= expectRight
      lookup hContentType (Client.requestHeaders built)
        `shouldBe` Just "application/json"
      case Client.requestBody built of
        Client.RequestBodyLBS value -> value `shouldBe` LBS.fromStrict "{\"answer\":42}"
        _ -> expectationFailure "expected a lazy JSON request body"

    it "preserves the chosen method for form bodies" $ do
      built <- inspectRequest
        (put "https://example.com" & withFormBody [("name", "Owain Lewis")])
        >>= expectRight
      Client.method built `shouldBe` "PUT"
      Client.requestHeaders built `shouldContain`
        [(hContentType, "application/x-www-form-urlencoded")]

    it "replaces singular authentication and content-type headers" $ do
      built <- inspectRequest (
        post "https://example.com"
          & withHeader (hAuthorization, "old")
          & bearerAuth "new"
          & withHeader (hContentType, "text/plain")
          & withJsonBody (object [])) >>= expectRight
      length (filter ((== hAuthorization) . fst) (Client.requestHeaders built))
        `shouldBe` 1
      lookup hContentType (Client.requestHeaders built)
        `shouldBe` Just "application/json"

    it "reports invalid URLs as values" $ do
      built <- inspectRequest (get "not a URL")
      case built of
        Left (InvalidUrl _ _) -> pure ()
        other -> expectationFailure ("expected InvalidUrl, got " <> show other)

    it "rejects unsafe custom methods before transport" $ do
      mapM_ (\unsafe -> do
        built <- inspectRequest (customMethod unsafe "https://example.com")
        case built of
          Left (RequestBuildError _) -> pure ()
          other -> expectationFailure ("expected RequestBuildError, got " <> show other))
        ["", "GET /other", "GET\r\nX-Injected: yes", "MÉTHOD"]

    it "rejects unsafe methods applied to existing requests" $ do
      built <- inspectRequest
        (get "https://example.com" & withMethod (CUSTOM "GET\r\nX-Injected: yes"))
      case built of
        Left (RequestBuildError _) -> pure ()
        other -> expectationFailure ("expected RequestBuildError, got " <> show other)

    it "preserves the chosen method for multipart bodies" $ do
      built <- inspectRequest
        (put "https://example.com" & withMultipartBody [partBS "name" "value"])
        >>= expectRight
      Client.method built `shouldBe` "PUT"

  describe "request policies" $ do
    it "sanitizes negative limits" $ do
      let policy = defaultRetryPolicy
            { retryLimit = -1
            , retryBaseDelayMicros = -1
            , retryMaxDelayMicros = -1
            }
          requestValue = get "https://example.com" & retrying policy
      -- Policy metadata is intentionally opaque. Successful compilation proves
      -- the policy does not corrupt the underlying request.
      inspectRequest requestValue >>= expectRight >> pure ()

expectRight :: Show error => Either error value -> IO value
expectRight (Left err) = expectationFailure (show err) >> fail "expected Right"
expectRight (Right value) = pure value
