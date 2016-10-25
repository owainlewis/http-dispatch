{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.RequestSpec where

import           Network.HTTP.Dispatch.Request
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "building GET requests" $ do
        it "constructs a GET request" $ do
            let actual = getRequest "http://google.com" []
                expected = HTTPRequest GET "http://google.com" [] Nothing
            actual `shouldBe` expected

    describe "building POST requests" $ do
        it "constructs a POST request" $ do
            let actual = postRequest "http://google.com" [] (Just "OK")
                expected = HTTPRequest POST "http://google.com" [] (Just "OK")
            actual `shouldBe` expected
