module Types (Action,Command(..),KeyBind(..)) where

import System.IO (Handle)
import qualified Data.Map.Lazy as Map

type Action = String
type Code = String
type Function = Handle -> IO ()


data Command = BasicCommand    Action Code
             | AdvancedCommand Action Function
             | ComponedCommand Action [Action]

data KeyBind = KeyBind Char Action

instance Show KeyBind where
  show (KeyBind k a) = (show k) ++ " -> " ++ a
