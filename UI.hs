module UI (goLineAndClear,lastLine,termWidth,putLine,putLines,initScreen, clearFromCursor, initCmdLine,showInCmdLine,exeFromCommandLine,showInfos,showHelp,getInput) where

import System.Console.ANSI
import Data.List
import System.IO (Handle(..),hSetBuffering,BufferMode(NoBuffering),hClose,stdout,stdin, hSetEcho)
import qualified Data.Map.Lazy as Map
import System.Console.Haskeline
import TermSize
--import OnkyoActions
--import Types(KeyBind)
import Infos


-- Haskeline
searchCommand :: String -> [Completion]
--searchCommand str = map simpleCompletion $ filter (str `isPrefixOf`) commands
--  where commands = map (\ (KeyBind c a) -> a) myKeyBinds
searchCommand str = []

searchInput :: String -> [Completion]
searchInput str = []

commandSettings :: Settings IO
commandSettings = Settings
  { historyFile = Just ".cmdhist"
  , complete = completeWord Nothing " \t" $ return . searchCommand
  , autoAddHistory = True
  }
inputSettings :: Settings IO
inputSettings = Settings
  { historyFile = Just ".inputhist"
  , complete = completeWord Nothing " \t" $ return . searchInput
  , autoAddHistory = True
  }
--------------------------------


goLineAndClear n = do
  setCursorPosition n 0 >> clearLine

lastLine = do
  size <- getTermSize
  return $ fst size - 1
 
termWidth = do
  size <- getTermSize
  return $ snd size

putLine s = do
  putStr (s ++ "\n") >> clearLine

putLines [] = do return ()
putLines (x:xs) = do
  putLine x
  putLines xs
  
initScreen = do
  clearScreen
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  initCmdLine 
  hSetEcho stdin False

clearFromCursor = clearFromCursorToScreenEnd

initCmdLine = do
  l <- lastLine
  tW <- termWidth
  goLineAndClear (l-1)
  putStrLn $ replicate tW '_'
  goLineAndClear l

getFromCommandLine = do
  initCmdLine 
  hSetEcho stdin True
  line <- runInputT commandSettings $ getInputLine ":"
  hSetEcho stdin False
  scrollPageDown 1
  initCmdLine 
  case line of
    Nothing -> return ""
    Just a -> return a 
  
getInput = do
  l <- lastLine
  goLineAndClear l
  hSetEcho stdin True
  line <- runInputT inputSettings $ getInputLine " -- INSERT -- "
  hSetEcho stdin False
  case line of
    Just a -> return a
    Nothing -> return ""
  
showInCmdLine s = do 
  l <- lastLine
  goLineAndClear l
  putStr s 


showInfos i = do
  goLineAndClear 0
  putStr ((infosTime i)  ++ "  ")
  putLine (infosTitle i)
  putStr ((infosTrack i) ++ "  ")
  putStr ((infosAlbum i)  ++ " - ")
  putLine (infosArtist i)
  putLines $ zipWith (++) (infosCursor i) (infosLines i)

exeFromCommandLine h map = do
  line <- getFromCommandLine
  case line of
    "" -> return ()
    a ->
      case Map.lookup a map of
        Just f -> f h
        Nothing -> showInCmdLine "Command not found"

showHelp h k = do
  let l = 20
  last <- lastLine
  goLineAndClear l
  putLines $ map show k
  goLineAndClear last
  putStr " -- Press a key to continue -- "
  c <- getChar
  goLineAndClear l
  clearFromCursor
  goLineAndClear last
  --case c of
  --  ':' -> exeFromCommandLine h aM
  --  _ -> return ()
