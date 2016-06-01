module Network.HTTP.Dispatch.IntegrationSpec where

import           Network.HTTP.Dispatch.Core
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "making GET requests" $ do
        it "returns HTTP 200" $ do
            response <- runRequest $ get "https://httpbin.org/get"
            print response
            (respStatus response) `shouldBe` 200
