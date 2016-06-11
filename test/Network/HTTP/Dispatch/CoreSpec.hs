{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.CoreSpec where

import           Network.HTTP.Dispatch.Core
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "building GET requests" $ do
        it "constructs a GET request" $ do
            let actual = get "http://google.com"
                expected = HTTPRequest GET "http://google.com" [] Nothing
            actual `shouldBe` expected
        it "constructs a GET request with headers" $ do
            let actual = getWithHeaders "http://google.com" [("Content-Type", "application/json")]
                expected = HTTPRequest GET "http://google.com" [("Content-Type", "application/json")] Nothing
            actual `shouldBe` expected

    describe "building POST requests" $ do
        it "constructs a POST request" $ do
            let actual = post "http://google.com" "OK"
                expected = HTTPRequest POST "http://google.com" [] (Just "OK")
            actual `shouldBe` expected
