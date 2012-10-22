module Listen (listen) where

import Data.Char (digitToInt)
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
    "NTM" -> infos {infosTime = cmdParam}
    "NAT" -> infos {infosArtist = cmdParam}
    "NAL" -> infos {infosAlbum = cmdParam}
    "NTI" -> infos {infosTitle = cmdParam}
    "NTR" -> infos {infosTrack = cmdParam}
    "MVL" -> infos {infosVolume = cmdParam}
    "AMT" -> infos {infosMute = cmdParam}
    "SPL" -> infos {infosTime = cmdParam}
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
            (l:xs)   -> -- cursor is present
              case xs of -- update type
                ('P':xs) -> infos {infosCursor = (cursorString (-1))}
                                  {infosLines = (infosLines infosEmpty)}
                ('C':xs) -> infos {infosCursor = (cursorString $ digitToInt l)}
        ('U':xs) -> infos {infosLines = (prevLines ++ line ++ nextLines)}
          where idx = digitToInt $ head xs
                subParam = tail xs
                prevLines = take idx $ infosLines infos
                line = [(take 1 subParam) ++ " " ++ (drop 1 subParam)]
                nextLines =  drop (idx+1) $ infosLines infos
        _ -> infos
    _ -> infos
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
  , infosLines = replicate 10 ""
  , infosCursor = cursorString (-1)
}

cursorString i =
  if (i == -1)
    then map space $ map show [0..9]
    else (map space (map show [0..(i-1)])) ++ ["-> "] ++
      (map space (map show [(i+1)..9]))
    where space a = a ++ "  "



loop h infos = do
  showInfos infos
  code <- getCodeFromOnkyo h
  --putStrLn $ "packet : " ++ line
  --putStrLn $ "message : " ++ msg
  loop h (updateInfos infos code)

listen h = do
  loop h infosEmpty
