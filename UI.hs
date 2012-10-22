module UI ( goLineAndClear, lastLine, termWidth, putLine, putLines, initScreen
          , clearFromCursor, initCmdLine, showInCmdLine, exeFromCommandLine
          , showInfos, showHelp, getInput) where

import System.Console.ANSI
import Data.List
import System.IO ( Handle(..), hSetBuffering, BufferMode(NoBuffering)
                 , hClose, stdout, stdin, hSetEcho)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import System.Console.Haskeline
import TermSize
import Infos
import Config
import Types


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
  
put3ColumnsLines [] = do return ()
put3ColumnsLines (x:y:z:xs) = do
  putLine $ x ++ "\t\t" ++ y ++ "\t\t" ++ z
  put3ColumnsLines xs

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

trim s =
  Text.unpack $ Text.strip $ Text.pack s

getFromCommandLine l = do
  initCmdLine 
  hSetEcho stdin True
  line <- runInputT (commandSettings l) $ getInputLine ":"
  hSetEcho stdin False
  scrollPageDown 1
  initCmdLine 
  case line of
    Nothing -> return ""
    Just a -> return $ trim a 
  
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

exeFromCommandLine h map l = do
  line <- getFromCommandLine l
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
  --putLines $ map show k
  put3ColumnsLines $ map show k
  goLineAndClear last
  putStr " -- Press a key to continue -- "
  c <- getChar
  goLineAndClear l
  clearFromCursor
  goLineAndClear last
