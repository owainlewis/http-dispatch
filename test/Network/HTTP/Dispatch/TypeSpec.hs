module Network.HTTP.Dispatch.TypeSpec where

import           Network.HTTP.Dispatch.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

getRequest :: HTTPRequest
getRequest = HTTPRequest GET "http://google.com" [] Nothing

spec :: Spec
spec = do
    describe "withMethod" $ do
        it "should add a header to a request" $ do
            let actual = withMethod getRequest POST
                expected = HTTPRequest POST "http://google.com" [] Nothing
            actual `shouldBe` expected

    describe "withHeader" $ do
        it "should add a header to a request" $ do
            let actual = withHeader getRequest ("Content-Type", "application/json")
                expected = HTTPRequest GET "http://google.com" [("Content-Type", "application/json")] Nothing
            actual `shouldBe` expected

        it "should add headers to a request" $ do
            let contentTypeHeader = ("Content-Type", "application/json")
                authHeader = ("Authorization", "Bearer 123")
                actual = withHeaders getRequest [contentTypeHeader, authHeader]
                expected = HTTPRequest GET "http://google.com" [contentTypeHeader, authHeader] Nothing
            actual `shouldBe` expected
