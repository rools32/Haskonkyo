import qualified Data.Map.Lazy as Map
import Network
import Data.Char (toLower, digitToInt)
import Text.Regex.Posix ((=~))
import Text.Printf
import System.IO (hSetBuffering,BufferMode(..),Handle,hClose,stdout,stdin, hSetEcho)
import System.IO.UTF8 (hGetLine,hPutStrLn)
import System.Exit
import Control.Concurrent (forkIO)

--port = 80
--ip = "74.125.230.248"
port = 60128
ip = "192.168.1.3"


type Action = String
type Code = String
type Function = (Handle -> IO ())
data Command = BasicCommand    Action Code
                  | AdvancedCommand Action Function

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
  AdvancedCommand "help" (\h -> putStrLn $ show keyBinds),
  --AdvancedCommand "showInfos" (\h -> showInfos h),
  AdvancedCommand "keyboard" (\h -> keyboardInput h)
  ]

commandToFunction (BasicCommand a c) = (a, (\h -> sendCode h c))
commandToFunction (AdvancedCommand a f) = (a, f)

actionMap = Map.fromList (map commandToFunction commands)

keyboardInput h = do
  hSetEcho stdin True
  putStr " -- INSERT -- "
  a <- getLine
  hSetEcho stdin False
  sendCode h ("NKY" ++ a)

type Key = Char
data KeyBind = KeyBind Key Action deriving Show
keyBinds = [
  KeyBind 'z' "poweroff",
  KeyBind 'a' "poweron",
  KeyBind ' ' "pause",
  KeyBind 'j' "down",
  KeyBind 'k' "up",
  KeyBind 'q' "quit",
  KeyBind 'h' "left",
  KeyBind 'l' "right",
  KeyBind 'n' "net",
  KeyBind 't' "top",
  KeyBind '\n'"enter",
  KeyBind 'u' "back",
  KeyBind '>' "next",
  KeyBind '<' "previous",
  KeyBind '+' "volume_up",
  KeyBind '-' "volume_down",
  KeyBind '0' "0",
  KeyBind '1' "1",
  KeyBind '2' "2",
  KeyBind '3' "3",
  KeyBind '4' "4",
  KeyBind '5' "5",
  KeyBind '6' "6",
  KeyBind '7' "7",
  KeyBind '8' "8",
  KeyBind '9' "9",
  KeyBind ':' "command",
  KeyBind 'i' "keyboard"
  ]
keyMap = Map.fromList (map (\(KeyBind k a) -> (k, Map.lookup a actionMap)) keyBinds)

getCmd h map = do
  putStr ":"
  hSetEcho stdin True
  a <- getLine
  hSetEcho stdin False
  case Map.lookup a map of
    Just f -> f h
    Nothing -> putStrLn "oups"

-- pack
pack a = "ISCP\x00\x00\x00\x10\x00\x00\x00" ++
           (printf "%c" $ length a + 3) ++
           "\x01\x00\x00\x00!1" ++ a ++ "\r"

-- uourceck a = drop 18 anpack
unpack a = drop 18 $ init $ init a

-- sendCode
sendCode h code = do
  hPutStrLn h $ pack code
  --putStrLn $ pack code


-- speak
speak h = do
  key <- getChar
  case Map.lookup key keyMap of
    Just (Just f) -> f h
    Just Nothing -> putStrLn "oups"
    Nothing -> putStrLn "oups"
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
  infosTime = "00:00",
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

showInfos i = do
  --putStrLn "Yeah"
  putStrLn (infosAlbum i)
  putStrLn (infosTime i)
  putStrLn $ unlines $ zipWith (++) (infosCursor i) (infosLines i)


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
  
listen h infos = do
  showInfos infos
  line <- hGetLine h
  --putStrLn $ "packet : " ++ line
  let msg = (unpack line)
  --putStrLn $ "message : " ++ msg
  listen h (updateInfos infos msg)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- fix buffering under windows
  hSetBuffering stdin NoBuffering -- fix buffering under windows
  hSetEcho stdin False

  h <- connectTo ip (PortNumber port)
  hSetBuffering h LineBuffering
  forkIO $ speak h
  listen h infosEmpty
