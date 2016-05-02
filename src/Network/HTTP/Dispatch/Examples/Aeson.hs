{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Aeson where

import           Data.Aeson
import           Network.HTTP.Dispatch.Core

data User = User { firstName :: String
                 , lastName  :: String
                 , age       :: Int } deriving ( Show )

instance ToJSON User where
    toJSON (User first last age) = object [ "first_name" .= first
                                          , "last_name"  .= last
                                          , "age" .= age
                                          ]

-- Create a POST request with the request body as JSON
request :: HTTPRequest
request = postAeson "http://requestb.in/shxmxvsh" headers body
    where headers = [("Content-Type", "application/json")]
          body = User "Jack" "Dorsey" 30

response :: IO HTTPResponse
response = runRequest request
