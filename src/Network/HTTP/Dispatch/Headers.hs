{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Headers
       ( contentType
       , contentJSON
       , contentXML
       , basicAuth
       ) where

import qualified Data.ByteString             as S
import qualified Data.ByteString.Base64      as B64
import           Data.Monoid                 ((<>))
import           Network.HTTP.Dispatch.Types

-- | Create a new content type header
contentType :: String -> Header
contentType = header "Content-Type"

-- | Return a JSON content type header
contentJSON :: Header
contentJSON = contentType "application/json"

-- | Return an XML content type header
contentXML :: Header
contentXML  = contentType "application/xml"

-- | Helper to generate Basic authentication
basicAuth :: S.ByteString -> S.ByteString -> Header
basicAuth user pass = ("Authorization", auth)
    where auth = "Basic " <> userPassEncoded
          userPassEncoded = B64.encode $ user <> ":" <> pass
