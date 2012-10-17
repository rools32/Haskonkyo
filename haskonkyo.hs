import qualified Data.Map.Lazy as Map
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Network
import Data.Char (toLower, digitToInt)
import Text.Regex.Posix ((=~))
import Text.Printf
import System.IO (hSetBuffering,BufferMode(..),Handle,hClose,stdout,stdin, hSetEcho)
import System.IO.UTF8 (hGetLine,hPutStrLn)
import System.Exit
import Control.Concurrent (forkIO, threadDelay)

--port = 80
--ip = "74.125.230.248"
port = 60128
ip = "192.168.1.3"

waitTime = 800000

type Action = String
type Code = String
type Function = (Handle -> IO ())
data Command = BasicCommand    Action Code
             | AdvancedCommand Action Function
             | ComponedCommand Action [Action]

commands = [
  BasicCommand "poweroff" "PWR00",
  BasicCommand "poweron" "PWR01",
  BasicCommand "pause" "NTCPAUSE",
  BasicCommand "down" "NTCDOWN",
  BasicCommand "up" "NTCUP",
  BasicCommand "left" "NTCLEFT",
  BasicCommand "right" "NTCRIGHT",
  BasicCommand "net" "SLI2B",
  BasicCommand "enter" "NTCSELECT",
  BasicCommand "back" "NTCRETURN",
  BasicCommand "next" "NTCTRUP",
  BasicCommand "previous" "NTCTRDOWN",
  BasicCommand "volume_up" "MVLUP",
  BasicCommand "volume_down" "MVLDOWN",
  BasicCommand "top" "NTCTOP",
  BasicCommand "0" "NLSL0",
  BasicCommand "1" "NLSL1",
  BasicCommand "2" "NLSL2",
  BasicCommand "3" "NLSL3",
  BasicCommand "4" "NLSL4",
  BasicCommand "5" "NLSL5",
  BasicCommand "6" "NLSL6",
  BasicCommand "7" "NLSL7",
  BasicCommand "8" "NLSL8",
  BasicCommand "9" "NLSL9",
  AdvancedCommand "quit" (\h -> hClose h >> exitSuccess),
  AdvancedCommand "command" (\h -> getCmd h actionMap),
  AdvancedCommand "help" (\h -> showHelp keyBinds),
  AdvancedCommand "refresh" (\h -> erase),
  AdvancedCommand "keyboard" (\h -> keyboardInput h),
  ComponedCommand "search" ["net", "3", "0", "keyboard"]
  ]

showHelp a = do
  move 0 0
  wAddStr stdScr $ unlines $ map show a
  refresh
  a <- getCh
  initWindow 
  return ()

commandToFunction (BasicCommand a c) = (a, (\h -> sendCode h c))
commandToFunction (AdvancedCommand a f) = (a, f)
commandToFunction (ComponedCommand a l) = (a, (\h -> sendActions h l))

actionMap = Map.fromList (map commandToFunction commands)

-- sendCode
sendCode h code = do
  hPutStrLn h $ pack code
  --putStrLn $ pack code

sendActions h actions = do
  if actions == []
    then return ()
    else
      case Map.lookup (head actions) actionMap of
        Just code -> do
          code h
          threadDelay waitTime
          sendActions h (tail actions)
        Nothing -> return ()
    


keyboardInput h = do
  size <- scrSize
  let termHeight = fst size
  move (termHeight-1) 0
  wAddStr stdScr " -- INSERT -- "
  a <- getCmdLine
  wAddStr stdScr a
  refresh
  sendCode h ("NKY" ++ a)

instance Ord Key where
  compare a b = compare (show a) (show b)

data KeyBind = BasicKeyBind Char Action
             | AdvancedKeyBind Key Action

instance Show KeyBind where
  show (BasicKeyBind k a) = (show k) ++ " -> " ++ a
  show (AdvancedKeyBind k a) = (show k) ++ " -> " ++ a

keyBinds = [
  BasicKeyBind 'z'  "poweroff",
  BasicKeyBind 'a'  "poweron",
  BasicKeyBind ' '  "pause",
  BasicKeyBind 'j'  "down",
  BasicKeyBind 'k'  "up",
  BasicKeyBind 'q'  "quit",
  BasicKeyBind 'h'  "left",
  BasicKeyBind 'l'  "right",
  BasicKeyBind 'n'  "net",
  BasicKeyBind 't'  "top",
  BasicKeyBind '\r' "enter",
  BasicKeyBind 'u'  "back",
  BasicKeyBind '>'  "next",
  BasicKeyBind '<'  "previous",
  BasicKeyBind '+'  "volume_up",
  BasicKeyBind '-'  "volume_down",
  BasicKeyBind '0'  "0",
  BasicKeyBind '1'  "1",
  BasicKeyBind '2'  "2",
  BasicKeyBind '3'  "3",
  BasicKeyBind '4'  "4",
  BasicKeyBind '5'  "5",
  BasicKeyBind '6'  "6",
  BasicKeyBind '7'  "7",
  BasicKeyBind '8'  "8",
  BasicKeyBind '9'  "9",
  BasicKeyBind ':'  "command",
  BasicKeyBind 'i'  "keyboard",
  BasicKeyBind 'r'  "refresh",
  AdvancedKeyBind KeyUp  "keyboard"
  ]
keyMap = Map.fromList $ map f keyBinds
  where f (BasicKeyBind k a) = (KeyChar k, Map.lookup a actionMap)
        f (AdvancedKeyBind k a) = (k, Map.lookup a actionMap)


initWindow = do
  initCurses
  wclear stdScr
  initCmdLine
  

initCmdLine = do
  size <- scrSize
  let termHeight = fst size
  move (termHeight-2) 0
  let termWidth = snd size
  clrToEol
  drawLine termWidth $ replicate termWidth hLine
  clearLine 
  refresh

showInCmdLine s = do 
  size <- scrSize
  let termHeight = fst size
  move (termHeight-1) 0
  wAddStr stdScr s 
  refresh

clearLine = do 
  size <- scrSize
  pos <- getYX stdScr
  move (fst pos) 0
  clrToEol  
  refresh

backSpace = do
  pos <- getYX stdScr
  if (snd pos > 1)
    then move (fst pos) (snd pos-1) >> clrToEol >> refresh 
    else return ()

getCmdLine = do
  refresh
  getCmdLine_ ""
  where
    getCmdLine_ s = do
      c <- getCh 
      let stringC = (displayKey c)
      case c of
        KeyChar '\r' -> clearLine >> return s
        KeyChar '\n' -> clearLine >> return s
        KeyChar '\ESC' -> clearLine >> return ""
        KeyChar '\DEL' -> backSpace >> getCmdLine_ (init s)
        --"\DEL" -> backSpace >> getCmdLine_ (init s)
        _ -> wAddStr stdScr stringC >> refresh >> getCmdLine_ (s ++ stringC)

getCmd h map = do
  initCmdLine 
  wAddStr stdScr ":"   
  a <- getCmdLine
  case Map.lookup a map of
    Just f -> f h
    Nothing -> showInCmdLine "Command not found"

-- pack
pack a = "ISCP\x00\x00\x00\x10\x00\x00\x00" ++
           (printf "%c" $ length a + 3) ++
           "\x01\x00\x00\x00!1" ++ a ++ "\r"

-- uourceck a = drop 18 anpack
unpack a = drop 18 $ init $ init a

  

-- speak
speak h = do
  key <- getCh
  case Map.lookup key keyMap of
    Just (Just f) -> f h
    Just Nothing -> showInCmdLine "Command not found"
    Nothing ->  showInCmdLine "Key not found"
  speak h



--type Source = "" | "dlna" | "vtuner" | "net"
--type Shuffle = ""
--type Repeat = ""
--type State = ""

data Infos = Infos {
  infosSource :: String,
  infosTime :: String,
  infosAlbum :: String,
  infosArtist :: String,
  infosTitle :: String,
  infosTrack :: String,
  infosVolume :: String,
  infosMute :: String,
  infosState :: String,
  infosRepeat :: String,
  infosShuffle :: String,
  infosLines :: [String],
  infosCursor :: [String]
  }

infosEmpty = Infos {
  infosSource = "",
  infosTime = "00:00/00:00",
  infosAlbum = "",
  infosArtist = "",
  infosTitle  = "",
  infosTrack = "",
  infosVolume = "",
  infosMute = "",
  infosState = "",
  infosRepeat = "",
  infosShuffle = "",
  infosLines = replicate 10 "",
  infosCursor = cursorString (-1)
}

showInfos (i, w) = do
  move 0 0
  wAddStr stdScr ((infosTime i)  ++ "  ")
  wAddStr stdScr (infosTitle i) >> addLn
  wAddStr stdScr ((infosTrack i) ++ "  ")
  wAddStr stdScr ((infosAlbum i)  ++ " - ")
  wAddStr stdScr (infosArtist i) >> addLn
  wAddStr stdScr $ unlines $ zipWith (++) (infosCursor i) (infosLines i)
  refresh


cursorString i =
  if (i == -1)
    then map space $ map show [0..9]
    else (map space (map show [0..(i-1)])) ++ ["-> "] ++
      (map space (map show [(i+1)..9]))
    where space a = a ++ "  "


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
  
listen h (infos, window) = do
  showInfos (infos, window)
  line <- hGetLine h
  --putStrLn $ "packet : " ++ line
  let msg = (unpack line)
  --putStrLn $ "message : " ++ msg
  listen h ((updateInfos infos msg), window)


main :: IO ()
main = do
  initWindow
  keypad stdScr True
  hSetBuffering stdout NoBuffering -- fix buffering under windows
  hSetBuffering stdin NoBuffering -- fix buffering under windows
  hSetEcho stdin False
  echo False
  nl False

  h <- connectTo ip (PortNumber port)
  hSetBuffering h LineBuffering
  forkIO $ speak h
  --listen h (infosEmpty, initCurses)
  listen h (infosEmpty, stdScr)
  endWin
