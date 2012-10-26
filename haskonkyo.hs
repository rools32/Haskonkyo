import Control.Concurrent (forkIO, newEmptyMVar, putMVar)
import OnkyoIO (connectToOnkyo)
import Speak
import Listen
import UI
import Config(keyList)
import Commands(commandList)

--port = 80
--ip = "74.125.230.248"
port = 60128
ip = "192.168.1.3"

main :: IO ()
main = do
  display <- newEmptyMVar
  putMVar display []
  h <- connectToOnkyo ip port
  initScreen
  forkIO $listen h display
  speak h display
