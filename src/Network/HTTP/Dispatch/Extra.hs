{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Extra
       ( contentType
       ) where

import qualified Data.ByteString        as S
import qualified Data.ByteString.Base64 as B64
import           Data.Monoid            ((<>))

-- | Headers
--
contentType :: S.ByteString -> (S.ByteString, S.ByteString)
contentType ct = ("Content-Type", ct)

-- | Basic authentication header helper
--
basicAuth :: S.ByteString -> S.ByteString -> (S.ByteString, S.ByteString)
basicAuth user pass = ("Authorization", auth)
    where auth = "Basic: " <> userPassEncoded
          userPassEncoded = B64.encode $ user <> ":" <> pass
