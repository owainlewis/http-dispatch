module Network.HTTP.Dispatch.RequestSpec where

import           Network.HTTP.Dispatch.Request
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "compileParams" $ do
        it "generate a valid query param string" $ do
            let actual = compileParams [("foo", "bar"), ("baz", "foo")]
                expected = "?foo=bar&baz=foo"
            actual `shouldBe` expected
