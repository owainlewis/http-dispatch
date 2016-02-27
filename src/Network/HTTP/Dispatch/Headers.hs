{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Headers where

import           Network.HTTP.Types.Header

acceptJSON :: Header
acceptJSON = (hAccept, "application/json")
