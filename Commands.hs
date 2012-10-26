module Commands (mapAction,sendActions,help,quit,command,commandList,
                 refreshInfos) where

import Network
import System.IO
import System.Exit
import Control.Concurrent (threadDelay)
import qualified Data.Map.Lazy as Map
import OnkyoIO (sendCodeToOnkyo)
import UI (exeFromCommandLine,getInput,showHelp,wipeScreen)
import Types (Command(..))
import Config (keyList)

commandToFunction (BasicCommand a c) = (a, (\ h d -> sendCodeToOnkyo h c))
commandToFunction (AdvancedCommand a f) = (a, f)
commandToFunction (ComponedCommand a l) = (a, (\ h d -> sendActions h d l))

mapAction l =
  Map.fromList (map commandToFunction l)

commandList =
  [ BasicCommand "poweroff" "PWR00"
  , BasicCommand "poweron" "PWR01"
  , BasicCommand "mute" "AMTTG"
  , BasicCommand "cbl-sat" "SLI01"
  , BasicCommand "net" "SLI2B"
  , BasicCommand "dlna" "NSV000"
  , BasicCommand "vtuner" "NSV020"
  , BasicCommand "dvd" "SLI10"
  , BasicCommand "spotify" "NSV0A1"
  , BasicCommand "pause" "NTCPAUSE"
  , BasicCommand "down" "NTCDOWN"
  , BasicCommand "up" "NTCUP"
  , BasicCommand "display" "NTCDISPLAY"
  , BasicCommand "left" "NTCLEFT"
  , BasicCommand "right" "NTCRIGHT"
  , BasicCommand "enter" "NTCSELECT"
  , BasicCommand "back" "NTCRETURN"
  , BasicCommand "next" "NTCTRUP"
  , BasicCommand "previous" "NTCTRDOWN"
  , BasicCommand "volume_up" "MVLUP"
  , BasicCommand "volume_down" "MVLDOWN"
  , BasicCommand "top" "NTCTOP"
  , BasicCommand "artist" "NATQSTN"
  , BasicCommand "album" "NALQSTN"
  , BasicCommand "title" "NTIQSTN"
  , BasicCommand "track" "NTRQSTN"
  , BasicCommand "list" "NTCLIST"
  , BasicCommand "0" "NLSL0"
  , BasicCommand "1" "NLSL1"
  , BasicCommand "2" "NLSL2"
  , BasicCommand "3" "NLSL3"
  , BasicCommand "4" "NLSL4"
  , BasicCommand "5" "NLSL5"
  , BasicCommand "6" "NLSL6"
  , BasicCommand "7" "NLSL7"
  , BasicCommand "8" "NLSL8"
  , BasicCommand "9" "NLSL9"
  , AdvancedCommand "quit" quit
  , AdvancedCommand "command" command
  , AdvancedCommand "help" help
  , AdvancedCommand "nop" nop
  , AdvancedCommand "keyboard" sendInput
  , AdvancedCommand "refreshInfos" refreshInfos
  , ComponedCommand "searchOnSpotify" ["spotify", "0", "keyboard"]
  , ComponedCommand "streaming" ["net", "0", "nop", "nop", "nop", "2", "0"]
  ]

command h display = do
  exeFromCommandLine h commandMap commandList display

help h display = do
  showHelp h keyList display

refreshList = ["artist", "album", "title", "track", "list"]

refreshInfos h display =
  sendActions h display refreshList

waitTime = 800000

nop h display = do
  return ()

commandMap =
  mapAction commandList 

sendActions h display actions = do
  if actions == []
    then return ()
    else
      case Map.lookup (head actions) commandMap of
        Just fun -> do
          fun h display
          threadDelay waitTime
          sendActions h display (tail actions)
        Nothing -> return ()
    
sendInput h display = do
  s <- getInput display
  case s of
    "" -> return ()
    _  -> sendCodeToOnkyo h ("NKY" ++ s)

quit h display = do
  hClose h >> exitSuccess

