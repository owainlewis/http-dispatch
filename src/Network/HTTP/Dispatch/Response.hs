{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.HTTP.Dispatch.Response
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP request generation DSL
--
module Network.HTTP.Dispatch.Response
    ( HTTPResponse(..)
    ) where

import qualified Data.ByteString       as S

data HTTPResponse = HTTPResponse {
    status  :: Int
  , headers :: [(S.ByteString, S.ByteString)]
  , body    :: S.ByteString
} deriving ( Eq, Show )
