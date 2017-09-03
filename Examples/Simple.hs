{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Dispatch.Examples.Simple where

import           Network.HTTP.Dispatch as Dispatch

example1 =
  let req = Dispatch.get "http://google.com" in
  Dispatch.http req
