{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Headers where

import           Network.HTTP.Dispatch.Types (Header (..))

contentJSON :: Header
contentJSON = Header "Content-Type" "application/json"
