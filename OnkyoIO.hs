module OnkyoIO (connectToOnkyo,sendCodeToOnkyo,getCodeFromOnkyo) where

import Network
import System.IO (hSetBuffering,BufferMode(LineBuffering),Handle)
import System.IO.UTF8 (hPutStrLn,hGetLine)
import Text.Printf

connectToOnkyo ip port = do
  h <- connectTo ip (PortNumber port)
  hSetBuffering h LineBuffering
  return h

getCodeFromOnkyo :: Handle -> IO String
getCodeFromOnkyo h = do
  line <- hGetLine h
  return $ unpack line
    where unpack a = drop 18 $ init $ init a

-- sendCodeToOnkyo
sendCodeToOnkyo h code = do
  hPutStrLn h $ pack code
    where pack a = "ISCP\x00\x00\x00\x10\x00\x00\x00" ++ (printf "%c" $ length a + 3) ++ "\x01\x00\x00\x00!1" ++ a ++ "\r"
