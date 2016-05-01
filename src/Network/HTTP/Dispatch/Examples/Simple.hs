{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Simple where

import           Control.Applicative         ((<$>))
import           Network.HTTP.Dispatch.Core
import           Network.HTTP.Dispatch.Types

-- Making a simple GET request

getReq :: IO HTTPResponse
getReq = runRequest (simpleGet "http://owainlewis.com")

--- Get the status code from a response
status :: IO Int
status = do
  resp <- runRequest $ simpleGet "http://owainlewis.com"
  return (respStatus resp)

-- Making a simple POST request

postReq = runRequest $ postString "http://requestb.in/x8cnvfx8" headers "Hello, World!"
    where headers = [ ("Content-Type", "application/json") ]
