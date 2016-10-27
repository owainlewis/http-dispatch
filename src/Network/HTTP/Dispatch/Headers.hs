{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.HTTP.Dispatch.Headers
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP header utils
--
module Network.HTTP.Dispatch.Headers ( basicAuth ) where

import qualified Data.ByteString             as S
import qualified Data.ByteString.Base64      as B64
import           Data.Monoid                 ((<>))
import           Network.HTTP.Dispatch.Types(Header)

-- | Helper to generate Basic authentication
basicAuth :: S.ByteString -> S.ByteString -> Header
basicAuth user pass = ("Authorization", auth)
    where auth = "Basic " <> userPassEncoded
          userPassEncoded = B64.encode $ user <> ":" <> pass
