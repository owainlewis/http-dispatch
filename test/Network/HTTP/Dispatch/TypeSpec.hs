module Network.HTTP.Dispatch.TypeSpec where

import           Network.HTTP.Dispatch.Types
import           Test.Hspec

main :: IO ()
main = hspec spec

getRequest :: HTTPRequest
getRequest = HTTPRequest GET "http://google.com" [] Nothing

jsonHeader :: (String, String)
jsonHeader = ("Content-Type", "application/json")

spec :: Spec
spec = do
    describe "withHeader" $ do
        it "should add a header to a request" $ do
            let actual = withHeader getRequest jsonHeader
                expected = HTTPRequest GET "http://google.com" [jsonHeader] Nothing
            actual `shouldBe` expected
