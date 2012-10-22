import Control.Concurrent (forkIO)
import OnkyoIO (connectToOnkyo)
import Speak
import Listen
import UI
import Config(keyList)
import Commands(commandList,initInfos)

--port = 80
--ip = "74.125.230.248"
port = 60128
ip = "192.168.1.3"

main :: IO ()
main = do
  h <- connectToOnkyo ip port
  initScreen
  --initInfos h
  forkIO $listen h
  speak h
