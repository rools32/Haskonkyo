import Control.Concurrent (forkIO, newEmptyMVar, putMVar)
import OnkyoIO (connectToOnkyo)
import Speak
import Listen
import UI
import Infos(infosEmpty)
import Config(keyList)
import Commands(commandList)

--port = 80
--ip = "74.125.230.248"
port = 60128
ip = "192.168.1.3"

main :: IO ()
main = do
  mvarInfos <- newEmptyMVar
  putMVar mvarInfos infosEmpty
  h <- connectToOnkyo ip port
  initScreen
  forkIO $listen h mvarInfos
  speak h mvarInfos
