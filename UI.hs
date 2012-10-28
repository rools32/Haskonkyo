module UI ( goLineAndClear, lastLine, termWidth, putLine, putLines, initScreen
          , clearFromCursor, initCmdLine, showInCmdLine
          , exeFromCommandLine, getFromCommandLine 
          , showInfos, showHelp, wipeScreen) where

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

putColorLinesAt l xs cs = do
  goLine l
  putColorLines xs cs

putColorLines [] _ = do return ()
putColorLines (x:xs) (c:cs) = do
  setSGR c
  putLine x
  setSGR []
  putColorLines xs cs


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
  clearLine

wipeScreen l = do
  goLineAndClear l
  clearFromCursor
  initCmdLine

trim s =
  Text.unpack $ Text.strip $ Text.pack s


inputInCommandLine s settings mvarInfos = do
  varInfos <- takeMVar mvarInfos -- blocage de l'affichage
  initCmdLine 
  showCursor
  hSetEcho stdin True
  line <- runInputT settings $ getInputLine s
  scrollPageDown 1
  hSetEcho stdin False
  hideCursor
  wipeScreen bottom
  putMVar mvarInfos (varInfos {infosMod = allInfos})
    -- reprise de l'affichage et raffichage
  return line

getFromCommandLine mvarInfos s l = do
  line <- inputInCommandLine s l mvarInfos
  case line of
    Nothing -> return ""
    Just a -> return $ trim a 
  
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
      | l == 3 -> putLineAt l (infosPage i ++ "  " ++ (show $ infosLine i)
                               ++ "/" ++ (show $ infosSize i))
      | l == 4 -> drawLineAt l
      | l == 5 -> UI.showList i l
  if infosMod i == []
    then return ()
    else showInfos i {infosMod = tail $ infosMod i}

showList i l = do
  putColorLinesAt l (zipWith (++)
                        -- add the number of the line
                       (map (\ x -> x ++ " - ")
                       (map show [0..9])) (infosList i))
                    (zipWith (++)
                      -- underline the current line
                      (map (\ x -> [SetSwapForegroundBackground x]) (infosCursor i))
                      -- colorize the playing song
                      (map (\ x -> if x then [SetColor Foreground Dull Red] else [])
                           (infosPlay i)))
  drawLine


exeFromCommandLine h map l mvarInfos = do
  line <- getFromCommandLine mvarInfos ":" l 
  case line of
    "" -> return ()
    a ->
      case Map.lookup a map of
        Just f -> f h mvarInfos
        Nothing -> showInCmdLine "Command not found"

showHelp h k mvarInfos = do
  let l = 20
  last <- lastLine
  goLineAndClear l
  --putLines $ map show k
  put3ColumnsLines $ map show k
  goLineAndClear last
  putStr " -- Press a key to continue -- "
  c <- getChar
  redrawInfos h mvarInfos

redrawInfos h mvarInfos = do
  varInfos <- takeMVar mvarInfos
  wipeScreen 1
  putMVar mvarInfos (varInfos {infosMod = allInfos})
