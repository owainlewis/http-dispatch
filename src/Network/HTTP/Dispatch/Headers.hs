{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Headers where

type Hdr = (String, String)

contentJSON :: Hdr
contentJSON = ("Content-Type", "application/json")
