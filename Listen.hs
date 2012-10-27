module Listen (listen) where

import Data.Char (digitToInt)
import Control.Concurrent (putMVar, takeMVar)
import UI
import Infos
import OnkyoIO (getCodeFromOnkyo)

updateInfos infos code =
  case cmdType of
    "NLT" -> 
      case cmdParam of
        "00" -> infos {infosSource = "dlna"}
        "02" -> infos {infosSource = "vtuner"}
        "F3" -> infos {infosSource = "net"}
        _    -> infos {infosSource = "oups"}
    "NTM" -> infos {infosTime = cmdParam} {infosMod = [1]}
    "SPL" -> infos {infosTime = cmdParam} {infosMod = [1]}
    "NTI" -> infos {infosTitle = cmdParam} {infosMod = [1]}
    "NAT" -> infos {infosArtist = cmdParam} {infosMod = [2]}
    "NAL" -> infos {infosAlbum = cmdParam} {infosMod = [2]}
    "NTR" -> infos {infosTrack = cmdParam} {infosMod = [2]}
    "MVL" -> infos {infosVolume = cmdParam} {infosMod = [1]}
    "AMT" -> infos {infosMute = cmdParam} {infosMod = [1]}
    -- state
    "NST" ->
      (\ (cmdParam, infos) -> 
        case head cmdParam of
           '-' -> infos {infosRepeat = "off"}
           'S' -> infos {infosRepeat = "all"}
           'A' -> infos {infosRepeat = "album"}
           'F' -> infos {infosRepeat = "folder"}) $
          (\ (cmdParam, infos) -> (
            tail cmdParam,
            case head cmdParam of
               '-' -> infos {infosRepeat = "off"}
               'R' -> infos {infosRepeat = "all"}
               'F' -> infos {infosRepeat = "folder"}
               '1' -> infos {infosRepeat = "one"})) $
              ( \(cmdParam, infos) -> (
                tail cmdParam,
                case head cmdParam of
                   'P' -> infos {infosState = "play"}
                   'S' -> infos {infosState = "stop"}
                   'p' -> infos {infosState = "pause"}
                   'F' -> infos {infosState = "FF"}
                   'R' -> infos {infosState = "FR"})) (cmdParam, infos)
    -- List info
    "NLS" ->
      case cmdParam of
        ('C':xs) -> -- cursor info
          case xs of
            (l:xs)   -> -- cursor is present
              case xs of -- update type
                ('P':xs) -> infos {infosCursor = (idxToBool idx)}
                                  {infosList = (infosList infosEmpty)}
                                  {infosMod = [5]}
                ('C':xs) -> infos {infosCursor = (idxToBool idx)}
                                  {infosMod = [5]}
             where  idx = if l == '-' then (-1) else digitToInt l
        ('U':(l:(play:value))) ->
          infos {infosPlay = if play == '0'
                               then idxToBool idx
                               else notIdxToBool idx (infosPlay infos)}
                {infosList = take idx (infosList infos)
                          ++ [value]
                          ++ (drop (idx+1) (infosList infos))}
                {infosMod = [5]}
          where  idx = digitToInt l
        _ -> infos {infosMod = []}
    _ -> infos {infosMod = []}
    where (cmdType, cmdParam) = splitAt 3 code
  
notIdxToBool idx list =
  take idx list ++ [False] ++ (drop (idx+1) list)

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
  , infosCursor = idxToBool 0
  , infosPlay = idxToBool (-1)
  , infosMod = [1..5]
}

idxToBool i =
  idxToBool_ i []

idxToBool_ i l =
  if length l == 10
    then l
    else
      idxToBool_ (i+1)
                  (if (i == 9) then (True:l) else (False:l))

loop h infos display = do
  mod <- takeMVar display
  showInfos $ infos {infosMod = infosMod infos ++ mod}
  code <- getCodeFromOnkyo h
  putMVar display []
  loop h (updateInfos infos code) display

listen h display = do
  loop h infosEmpty display
