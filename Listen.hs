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
      (\(cmdParam, infos) -> 
        case head cmdParam of
           '-' -> infos {infosRepeat = "off"}
           'S' -> infos {infosRepeat = "all"}
           'A' -> infos {infosRepeat = "album"}
           'F' -> infos {infosRepeat = "folder"}) $
          (\(cmdParam, infos) -> (
            tail cmdParam,
            case head cmdParam of
               '-' -> infos {infosRepeat = "off"}
               'R' -> infos {infosRepeat = "all"}
               'F' -> infos {infosRepeat = "folder"}
               '1' -> infos {infosRepeat = "one"})) $
              (\(cmdParam, infos) -> (
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
            ('-':xs) -> infos {infosCursor = (cursorString (-1))}
                              {infosMod = [5]}
            (l:xs)   -> -- cursor is present
              case xs of -- update type
                ('P':xs) -> infos {infosCursor = (cursorString (-1))}
                                  {infosList = (infosList infosEmpty)}
                                  {infosMod = [5]}
                ('C':xs) -> infos {infosCursor = (cursorString $ digitToInt l)}
                                  {infosMod = [5]}
        ('U':xs) -> infos {infosList = (prevLines ++ line ++ nextLines)}
                          {infosMod = [5]}
          where idx = digitToInt $ head xs
                subParam = tail xs
                prevLines = take idx $ infosList infos
                line = [(take 1 subParam) ++ " " ++ (drop 1 subParam)]
                nextLines =  drop (idx+1) $ infosList infos
        _ -> infos {infosMod = []}
    _ -> infos {infosMod = []}
    where (cmdType, cmdParam) = splitAt 3 code
  
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
  , infosCursor = cursorString (-1)
  , infosMod = [1..5]
}

cursorString i =
  if (i == -1)
    then map space $ map show [0..9]
    else (map space (map show [0..(i-1)])) ++ ["-> "] ++
      (map space (map show [(i+1)..9]))
    where space a = a ++ "  "



loop h infos display = do
  mod <- takeMVar display
  showInfos $ infos {infosMod = infosMod infos ++ mod}
  code <- getCodeFromOnkyo h
  putMVar display []
  loop h (updateInfos infos code) display

listen h display = do
  loop h infosEmpty display
