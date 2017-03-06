{-# LANGUAGE OverloadedStrings    #-}
-- |
-- Module      : Network.HTTP.Dispatch.Headers
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- HTTP Types
--
module Network.HTTP.Dispatch.Types
    ( Header
    , Headers
    , Url
    ) where

import qualified Data.ByteString       as S

--------------------------------------------------------------

type Header  = (S.ByteString, S.ByteString)

type Headers = [Header]

type Url     = String

--------------------------------------------------------------

