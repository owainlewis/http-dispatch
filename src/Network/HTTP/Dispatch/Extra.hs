{-# LANGUAGE FlexibleInstances #-}
module Network.HTTP.Dispatch.Extra
       ( fromString
       ) where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC

-----------------------------------------------------------------------------------
-- Extra methods for friendly API (experimental)
-----------------------------------------------------------------------------------

-- Can be used to generate a HTTP request without needing to prepare Lazy ByteStrings.
--
-- Example:
--   HTTPRequest POST "http://api.mysite.com" [("Content-Type", "application/json")] (fromString "HELLO WORLD")
fromString :: String -> LBS.ByteString
fromString = LBSC.pack
