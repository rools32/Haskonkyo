module UI ( goLineAndClear, lastLine, termWidth, putLine, putLines, initScreen
          , clearFromCursor, initCmdLine, showInCmdLine, exeFromCommandLine
          , showInfos, showHelp, getInput,wipeScreen) where

import System.Console.ANSI
import Data.List
import Control.Concurrent (putMVar, takeMVar)
import System.IO ( Handle(..), hSetBuffering, BufferMode(NoBuffering)
                 , hClose, stdout, stdin, hSetEcho)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import System.Console.Haskeline
import TermSize
import Infos
import Config
import Types

allInfos :: [Int]
allInfos = [1..5]

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

goLine n = do
  setCursorPosition n 0

goLineAndClear n = do
  goLine n >> clearLine

lastLine = do
  size <- getTermSize
  return $ fst size - 1
 
termWidth = do
  size <- getTermSize
  return $ snd size

putLineAt l s = do
  goLine l
  putLine s

putLine s = do
  putStr s >> clearFromCursorToLineEnd 
  putStr "\n"

putLinesAt l x = do
  goLine l
  putLines x

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
  hideCursor
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  initCmdLine 
  hSetEcho stdin False

clearFromCursor = clearFromCursorToScreenEnd

drawLineAt l = do
  goLineAndClear l
  drawLine

drawLine = do
  tW <- termWidth
  putStrLn $ replicate tW '_'

initCmdLine = do
  l <- lastLine
  goLineAndClear $ l-1
  drawLine

wipeScreen l = do
  goLineAndClear l
  clearFromCursor
  initCmdLine
  --scrollPageDown 1

trim s =
  Text.unpack $ Text.strip $ Text.pack s


inputInCommandLine s settings display = do
  takeMVar display -- blocage de l'affichage
  initCmdLine 
  showCursor
  hSetEcho stdin True
  line <- runInputT settings $ getInputLine s
  hSetEcho stdin False
  hideCursor
  wipeScreen bottom
  putMVar display allInfos -- reprise de l'affichage et raffichage
  return line

getFromCommandLine l display = do
  line <- inputInCommandLine ":" (commandSettings l) display
  case line of
    Nothing -> return ""
    Just a -> return $ trim a 
  
getInput display = do
  line <- inputInCommandLine " -- INSERT -- " inputSettings display
  case line of
    Just a -> return a
    Nothing -> return ""
  
showInCmdLine s = do 
  l <- lastLine
  goLineAndClear l
  putStr s 

bottom = 17


showInfos i = do
  case infosMod i of
    [] -> return ()
    (l:ls)
      | l == 1 -> putLineAt l (infosTime i ++ "  " ++ (infosTitle i))
      | l == 2 -> putLineAt l (infosTrack i ++ "  " ++ (infosAlbum i)
                              ++ " - " ++ (infosArtist i))
      | l == 3 -> putLineAt l ""
      | l == 4 -> drawLineAt l
      | l == 5 -> putLinesAt l (zipWith (++) (infosCursor i) (infosList i))
                    >> drawLine
  if infosMod i == []
    then return ()
    else showInfos i {infosMod = tail $ infosMod i}

exeFromCommandLine h map l display = do
  line <- getFromCommandLine l display
  case line of
    "" -> return ()
    a ->
      case Map.lookup a map of
        Just f -> f h display
        Nothing -> showInCmdLine "Command not found"

showHelp h k display = do
  let l = 20
  last <- lastLine
  goLineAndClear l
  --putLines $ map show k
  put3ColumnsLines $ map show k
  goLineAndClear last
  putStr " -- Press a key to continue -- "
  c <- getChar
  redrawInfos h display

redrawInfos h display = do
  takeMVar display
  wipeScreen 1
  putMVar display [1..5]
