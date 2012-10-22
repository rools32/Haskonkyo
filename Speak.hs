module Speak (speak) where

import qualified Data.Map.Lazy as Map
import Control.Concurrent (threadDelay)

import UI
import Keys
import Commands
import Config

waitTime = 800000

commandMap =
  mapAction commandList 

keyMap =
 mapKeys keyList commandMap

speak h = do
  key <- getChar
  case Map.lookup key keyMap of
    Just (Just f) -> f h
    Just Nothing  -> showInCmdLine "Command not found"
    Nothing       -> showInCmdLine "Key not found"
  speak h
