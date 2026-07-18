{-# LANGUAGE OverloadedStrings #-}

-- | Deprecated helpers for incrementally moving v1 call sites to v2.
module Network.HTTP.Dispatch.Compat
  ( legacyPost
  , legacyPut
  , legacyPatch
  , legacyWithHeader
  , legacyWithHeaders
  , legacyWithBody
  , legacyWithMethod
  ) where

import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as Client

import Network.HTTP.Dispatch.Internal (HTTPRequest, RequestMethod)
import qualified Network.HTTP.Dispatch.Request as Request
import Network.HTTP.Dispatch.Types (Header, Headers, Url)

{-# DEPRECATED legacyPost "Use post url & withBody body." #-}
-- | V1-style POST constructor with an optional strict body.
legacyPost :: Url -> Maybe ByteString -> HTTPRequest
legacyPost target payload = maybe id Request.withBody payload (Request.post target)

{-# DEPRECATED legacyPut "Use put url & withBody body." #-}
-- | V1-style PUT constructor with an optional strict body.
legacyPut :: Url -> Maybe ByteString -> HTTPRequest
legacyPut target payload = maybe id Request.withBody payload (Request.put target)

{-# DEPRECATED legacyPatch "Use patch url & withBody body." #-}
-- | V1-style PATCH constructor with an optional strict body.
legacyPatch :: Url -> Maybe ByteString -> HTTPRequest
legacyPatch target payload = maybe id Request.withBody payload (Request.patch target)

{-# DEPRECATED legacyWithHeader "Argument order changed: use withHeader header request." #-}
-- | V1 request-first header modifier.
legacyWithHeader :: HTTPRequest -> Header -> HTTPRequest
legacyWithHeader requestValue value = Request.modifyClientRequest
  (\built -> pure built
    { Client.requestHeaders = value : Client.requestHeaders built })
  requestValue

{-# DEPRECATED legacyWithHeaders "Argument order changed: use withHeaders headers request." #-}
-- | V1 request-first multi-header modifier.
legacyWithHeaders :: HTTPRequest -> Headers -> HTTPRequest
legacyWithHeaders = flip Request.setHeaders

{-# DEPRECATED legacyWithBody "Argument order changed: use withBody body request." #-}
-- | V1 request-first strict-body modifier.
legacyWithBody :: HTTPRequest -> ByteString -> HTTPRequest
legacyWithBody = flip Request.withBody

{-# DEPRECATED legacyWithMethod "Argument order changed: use withMethod method request." #-}
-- | V1 request-first method modifier.
legacyWithMethod :: HTTPRequest -> RequestMethod -> HTTPRequest
legacyWithMethod = flip Request.withMethod
