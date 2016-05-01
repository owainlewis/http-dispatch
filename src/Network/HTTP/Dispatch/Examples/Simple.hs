module Network.HTTP.Dispatch.Examples.Simple where

import           Network.HTTP.Dispatch.Core
import           Network.HTTP.Dispatch.Types

-- Making a simple GET request

getReq = simpleGet "http://google.com"
