module Network.HTTP.Dispatch.IntegrationSpec where

import           Network.HTTP.Dispatch.Request
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "making GET requests" $ do
        it "sends the correct values" $ do
            pending
