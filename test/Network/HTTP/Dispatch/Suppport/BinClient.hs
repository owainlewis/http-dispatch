module Network.HTTP.Dispatch.Support.BinClient where

import           Data.Aeson
import           Network.HTTP.Dispatch.Core

-- | Utility client for testing against httpbin.org
--
url:: String
url = "https://httpbin.org"

resource :: String -> String
resource name = mconcat [url, "/", name]
