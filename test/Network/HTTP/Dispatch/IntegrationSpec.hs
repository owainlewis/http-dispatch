{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.IntegrationSpec where

import           Network.HTTP.Dispatch.Core
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "making GET requests" $ do
        it "returns HTTP 200" $ do
            response <- runRequest $ getRequest "https://httpbin.org/get" []
            (respStatus response) `shouldBe` 200
    describe "making POST requests" $ do
        it "returns HTTP 200" $ do
            response <- runRequest $ postRequest "https://httpbin.org/post" [] (Just "Hello, World!")
            (respStatus response) `shouldBe` 200
    describe "making POST requests" $ do
        it "returns HTTP 200" $ do
            response <- runRequest $ deleteRequest "https://httpbin.org/delete" []
            (respStatus response) `shouldBe` 200
