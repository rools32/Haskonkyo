module Commands (mapAction,sendActions,help,quit,command,commandList,
                 refreshInfos) where

import Network
import System.IO
import System.Exit
import Control.Concurrent (threadDelay,putMVar, takeMVar)
import qualified Data.Map.Lazy as Map
import System.Console.Haskeline
import Data.List(isPrefixOf)

import OnkyoIO (sendCodeToOnkyo)
import UI (exeFromCommandLine,showHelp,wipeScreen,getFromCommandLine,showInCmdLine)
import Types (Command(..))
import Config (keyList)
import Infos

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
  , BasicCommand "forward" "NTCFF"
  , BasicCommand "backward" "NTCREW"
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
  , AdvancedCommand "searchOnList" inputSearchOnList
  , AdvancedCommand "refreshInfos" refreshInfos
  , ComponedCommand "searchOnSpotify" ["spotify", "0", "keyboard"]
  , ComponedCommand "streaming" ["net", "0", "nop", "nop", "nop", "2", "0"]
  , ComponedCommand "playlist" ["display", "back"]
  ]

command h mvarInfos = do
  exeFromCommandLine h commandMap (commandSettings commandList) mvarInfos

-- Haskeline
searchCommand l str =
  map simpleCompletion $ filter (str `isPrefixOf`) $ map commandToName l
    where commandToName (BasicCommand a c) = a
          commandToName (AdvancedCommand a f) = a
          commandToName (ComponedCommand a l) = a
commandSettings l = Settings
  { historyFile = Just ".cmdhist"
  , complete = completeWord Nothing " \t" $ return . searchCommand l
  , autoAddHistory = True
  }

searchInput str = []
inputSettings = Settings
  { historyFile = Just ".inputhist"
  , complete = completeWord Nothing " \t" $ return . searchInput
  , autoAddHistory = True
  }

searchSettings =
  Settings { historyFile = Just ".search"
           , complete = completeWord Nothing " \t" $ return . (\ s -> [])
           , autoAddHistory = True
           }

--------------------------------


help h mvarInfos = do
  showHelp h keyList mvarInfos

refreshList = ["artist", "album", "title", "track", "list"]

refreshInfos h mvarInfos =
  sendActions h mvarInfos refreshList

waitTime = 800000

nop h mvarInfos = do
  return ()

commandMap =
  mapAction commandList 

sendActions h mvarInfos actions = do
  if actions == []
    then return ()
    else
      case Map.lookup (head actions) commandMap of
        Just fun -> do
          fun h mvarInfos
          threadDelay waitTime
          sendActions h mvarInfos (tail actions)
        Nothing -> return ()
    
sendInput h mvarInfos = do
  s <- getFromCommandLine mvarInfos " -- INSERT -- " inputSettings
  case s of
    "" -> return ()
    _  -> sendCodeToOnkyo h ("NKY" ++ s)

quit h mvarInfos = do
  hClose h >> exitSuccess


inputSearchOnList h mvarInfos = do
  search <- getFromCommandLine mvarInfos "/" searchSettings
  varInfos <- takeMVar mvarInfos
  putMVar mvarInfos varInfos
  let  line = infosLine varInfos
  case search of
    "" -> return ()
    a  -> do
      found <- searchOnList h mvarInfos a line True
      if found then showInCmdLine "Found!"
               else showInCmdLine $ "\"" ++ a ++ "\" not found"

searchOnList h mvarInfos s line first = do
  varInfos <- takeMVar mvarInfos
  putMVar mvarInfos varInfos
  if not first && (infosLine varInfos == line)
    then return False -- one loop
    else if infosSearch varInfos s
           then return True
           else sendCodeToOnkyo h "NTCRIGHT"
             >> threadDelay 1000000
             >> searchOnList h mvarInfos s line False
