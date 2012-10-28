
module Infos (Infos(..)
           , infosEmpty
           , allInfos
           , infosSearch
           )
  where

import Data.List
import Text.Regex

data Infos = Infos
  { infosSource :: String
  , infosTime :: String
  , infosAlbum :: String
  , infosArtist :: String
  , infosTitle :: String
  , infosTrack :: String
  , infosVolume :: String
  , infosMute :: String
  , infosState :: String
  , infosRepeat :: String
  , infosShuffle :: String
  , infosList :: [String]
  , infosCursor :: [Bool]
  , infosPlay :: [Bool]
  , infosMod :: [Int]
  , infosLine :: Int
  , infosSize :: Int
  , infosPage :: String
  }

infosEmpty = Infos
  { infosSource = ""
  , infosTime = "00:00/00:00"
  , infosAlbum = "Album"
  , infosArtist = "Artist"
  , infosTitle  = "Title"
  , infosTrack = "00/00"
  , infosVolume = "00"
  , infosMute = "-"
  , infosState = "-"
  , infosRepeat = "-"
  , infosShuffle = "-"
  , infosList = replicate 10 ""
  , infosCursor = replicate 10 False
  , infosPlay = replicate 10 False
  , infosMod = allInfos
  , infosLine = 0
  , infosSize = 0 
  , infosPage  = "Page"
}


allInfos :: [Int]
allInfos = [1..5]

findOnString pattern s =
  case matchRegex (mkRegexWithOpts pattern False False) s of
    Just [] -> True
    Nothing -> False

infosSearch infos pattern =
  or $ map ( \ x -> findOnString pattern x) (infosList infos)

--      reduce op (x:xs) =
--        if xs == [] then x
--                    else op x (reduce op xs)
--
--
--      findOnString pattern s =
--        case findIndex (isPrefixOf pattern) (tails s) of
--          Just _  -> True
--          Nothing -> False
