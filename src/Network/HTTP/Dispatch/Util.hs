module Network.HTTP.Dispatch.Util where

getManagerForUrl :: (Eq a, Data.String.IsString [a]) => [a] -> IO Manager
getManagerForUrl url =
    if ("https" `isPrefixOf` url) then newManager tlsManagerSettings
                                  else newManager defaultManagerSettings
