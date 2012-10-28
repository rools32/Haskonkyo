module Listen (listen) where

import Data.Char (digitToInt)
import Control.Concurrent (putMVar, takeMVar)
import Numeric(readHex)

import UI
import Infos
import OnkyoIO (getCodeFromOnkyo)

subStrings [] s = [s]
subStrings (i:idx) s =
  [take i s] ++ (subStrings idx $ drop i s)

hexToInt = fst . head . readHex 

updateInfos infos code =
  case cmdType of
    "NLT" -> 
      case source of
        "00" -> infos {infosSource = "dlna"}
        "0A" -> infos {infosSource = "spotify"}
                      {infosLine = hexToInt globalLine}
                      {infosSize = hexToInt totalLine}
                      {infosPage = title}
                      {infosMod = [3]}
        "02" -> infos {infosSource = "vtuner"}
        "F3" -> infos {infosSource = "net"}
        _    -> infos {infosSource = "oups"}
      where [source, tmp1, globalLine, totalLine, tmp2, title] =
              subStrings [2, 2, 4, 4, 10] cmdParam
    "NTM" -> infos {infosTime = cmdParam} {infosMod = [1]}
    "SPL" -> infos {infosTime = cmdParam} {infosMod = [1]}
    "NTI" -> infos {infosTitle = cmdParam} {infosMod = [1]}
    "NAT" -> infos {infosArtist = cmdParam} {infosMod = [2]}
    "NAL" -> infos {infosAlbum = cmdParam} {infosMod = [2]}
    "NTR" -> infos {infosTrack = cmdParam} {infosMod = [2]}
    "MVL" -> infos {infosVolume = cmdParam} {infosMod = [1]}
    "AMT" -> infos {infosMute = cmdParam} {infosMod = [1]}
    -- state TODO correct
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
    -- List info TODO use subStrings
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
    where [cmdType, cmdParam] = subStrings [3] code
  
notIdxToBool idx list =
  take idx list ++ [False] ++ (drop (idx+1) list)

idxToBool i =
  idxToBool_ i []

idxToBool_ i l =
  if length l == 10
    then l
    else
      idxToBool_ (i+1)
                  (if (i == 9) then (True:l) else (False:l))

loop h infos mvarInfos = do
  -- Showing infos
  varInfos <- takeMVar mvarInfos
  let mod = infosMod varInfos -- lines to be refreshed
  showInfos $ infos {infosMod = infosMod infos ++ mod}
  putMVar mvarInfos infos

  -- Listen from Onkyo
  code <- getCodeFromOnkyo h

  loop h (updateInfos infos code) mvarInfos


listen h mvarInfos = do
  loop h infosEmpty mvarInfos
