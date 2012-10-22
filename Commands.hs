module Commands (mapAction,sendActions,exeFromCommandLine,help,quit,command,commandList) where

import Network
import System.IO
import System.Exit
import Control.Concurrent (threadDelay)
import qualified Data.Map.Lazy as Map
import OnkyoIO (sendCodeToOnkyo)
import UI (showInCmdLine,exeFromCommandLine,getInput,showHelp)
import Types (Command(..))
import Config (keyList)

commandToFunction (BasicCommand a c) = (a, (\ h -> sendCodeToOnkyo h c))
commandToFunction (AdvancedCommand a f) = (a, f)
commandToFunction (ComponedCommand a l) = (a, (\ t -> sendActions t l))

mapAction l =
  Map.fromList (map commandToFunction l)


commandList =
  [ BasicCommand "poweroff" "PWR00"
  , BasicCommand "poweron" "PWR01"
  , BasicCommand "pause" "NTCPAUSE"
  , BasicCommand "down" "NTCDOWN"
  , BasicCommand "up" "NTCUP"
  , BasicCommand "left" "NTCLEFT"
  , BasicCommand "right" "NTCRIGHT"
  , BasicCommand "net" "SLI2B"
  , BasicCommand "enter" "NTCSELECT"
  , BasicCommand "back" "NTCRETURN"
  , BasicCommand "next" "NTCTRUP"
  , BasicCommand "previous" "NTCTRDOWN"
  , BasicCommand "volume_up" "MVLUP"
  , BasicCommand "volume_down" "MVLDOWN"
  , BasicCommand "top" "NTCTOP"
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
  --, AdvancedCommand "refresh" (\h -> clearScreen)
  , AdvancedCommand "keyboard" sendInput
  , ComponedCommand "spotify" ["net", "3"]
  , ComponedCommand "searchOnSpotify" ["spotify", "0", "keyboard"]
  ]

command h = do
  exeFromCommandLine h $ mapAction commandList

help h = do
  showHelp h keyList

waitTime = 800000

commandMap =
  mapAction commandList 

sendActions h actions = do
  if actions == []
    then return ()
    else
      case Map.lookup (head actions) commandMap of
        Just code -> do
          code h
          threadDelay waitTime
          sendActions h (tail actions)
        Nothing -> return ()
    
sendInput h = do
  s <- getInput
  case s of
    "" -> return ()
    _  -> sendCodeToOnkyo h ("NKY" ++ s)

quit h = do
  hClose h >> exitSuccess
