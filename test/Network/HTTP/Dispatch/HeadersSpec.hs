{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.HeadersSpec where

import           Network.HTTP.Dispatch.Headers
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Constructing basic auth" $ do
        it "Constructs the correct header" $ do
            let actual = basicAuth "foo" "bar"
                expected = ("Authorization", "Basic Zm9vOmJhcg==")
            actual `shouldBe` expected
