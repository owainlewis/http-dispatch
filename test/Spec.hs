module Main (main) where

import qualified Network.HTTP.Dispatch.CoreSpec as CoreSpec
import qualified Network.HTTP.Dispatch.RequestSpec as RequestSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  RequestSpec.spec
  CoreSpec.spec
