-- | Public data types. Request and client constructors remain opaque so their
-- internals can evolve without breaking user code.
module Network.HTTP.Dispatch.Types
  ( Client
  , ClientOptions (managerSettings, clientProxyPolicy, useCookieJar)
  , ClientProxyPolicy (..)
  , defaultClientOptions
  , HTTPRequest
  , HTTPResponse (..)
  , HTTPError (..)
  , RequestMethod (..)
  , RetryPolicy (..)
  , defaultRetryPolicy
  , StatusPolicy (..)
  , Header
  , Headers
  , Url
  , Proxy (..)
  , CookieJar
  , BodyReader
  , Manager
  , ManagerSettings
  , RequestBody (..)
  , Part
  , resposeBody
  ) where

import Network.HTTP.Client
  ( BodyReader
  , CookieJar
  , Manager
  , ManagerSettings
  , Proxy (..)
  , RequestBody (..)
  )
import Network.HTTP.Client.MultipartFormData (Part)
import Network.HTTP.Types (Header)

import Network.HTTP.Dispatch.Internal

-- | Ordered request or response headers. Duplicate names are preserved.
type Headers = [Header]

-- | Absolute HTTP or HTTPS URL accepted by request constructors.
type Url = String

-- | Compatibility alias for the misspelled v1 accessor.
{-# DEPRECATED resposeBody "Use responseBody." #-}
resposeBody :: HTTPResponse body -> body
resposeBody = responseBody
