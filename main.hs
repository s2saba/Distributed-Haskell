import Data.Map as Map
import Distributed.Gossip as Gossip
import Distributed.Config
import Data.ConfigFile
import System.Time
import Control.Concurrent
import Network
import Network.Socket
import System.IO
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted

main = do 
  (TOD time _) <- getClockTime
  config <- getConfigOrFail
  let membermap = initialize config time
  mvar <- newMVar membermap
  blocker <- newEmptyMVar
  forkIO $ listenForGossip blocker mvar $ PortNumber 9092
  takeMVar blocker
  newmembermap <- takeMVar mvar
  return newmembermap
  
getConfigOrFail :: IO (ConfigParser)
getConfigOrFail = do
  config <- getConfig "Gossip.conf"
  case config of 
    Nothing -> error "Couldn't get config."
    Just conf -> return conf

--runUpdate :: MVar (Map ID Node) -> HostName -> Time -> Time -> IO TimerIO
--runUpdate membermap host joinTime time = do
--  repeatedStart (modifyMVar membermap (doUpdate host joinTime)) time

--doUpdate :: HostName -> Time -> Map ID Node -> IO ()
--doUpdate host joinTime m = do 
--  putStrLn m
--  updateMe m host joinTime

listenForGossip :: MVar String -> MVar (Map ID Node) -> PortID -> IO ()
listenForGossip blocker membermap port = do
  connected <- listenOn port
  setSocketOption connected ReuseAddr 1 
  (handle, hostname, port) <- Network.accept connected
  gotmap <- hGetContents handle
  modifyMVar membermap (\x -> return $ (Gossip.merge x $ read gotmap, ()))
  putMVar blocker "Done."

initialize :: ConfigParser -> Time -> Map ID Node
initialize conf time =
    let ip = getCrucial conf "gossip" "bindip" in
    updateMe Map.empty ip time
  
             
getCrucial :: (Get_C a) => ConfigParser -> String -> String -> a
getCrucial cp section key = 
    let val = (getValue cp section key) in
    case val of
      Left err -> error $ "Failed to get value from config: " ++ err
      Right v -> v      
