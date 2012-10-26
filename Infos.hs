module Infos (Infos(..)) where

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
  , infosCursor :: [String]
  , infosMod :: [Int]
  }

