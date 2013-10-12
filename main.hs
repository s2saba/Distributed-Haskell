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
import Data.Word
import Data.String.Utils

main = runGossip

runGossip = do
  (TOD time _) <- getClockTime
  config <- getConfigOrFail
  let membermap = initialize config time
  let ip = getCrucial config "gossip" "bindip"
  let port = getCrucial config "gossip" "contactport"
  mapmvar <- newMVar membermap
  tmr <- repeatedTimer (doUpdate ip time mapmvar) $ sDelay 1
  x <- newEmptyMVar
  listener <- forkIO $ listenForGossip mapmvar x $ PortNumber (PortNum port)
  takeMVar x
  return tmr

listenForGossip :: MVar (Map ID Node) -> MVar String -> PortID -> IO ()
listenForGossip mvarMap block port = do
  sock <- listenOn port
  putStrLn $ "Listening on port " ++ (show port)
  setSocketOption sock ReuseAddr 1 
  handleConnections sock mvarMap
  modifyMVar block (\x -> return ("Done.",()))
  return ()

handleConnections :: Socket -> MVar (Map ID Node) -> IO ()
handleConnections sock mvarMap = do
  putStrLn "Waiting for connections."
  (handle, hostname, port) <- Network.accept sock
  forkIO $ handleAccepted handle mvarMap
  handleConnections sock mvarMap

handleAccepted :: Handle -> MVar (Map ID Node) -> IO ()
handleAccepted handle mvarMap = do
  gotmap <- hGetContents handle
  case reads $ strip gotmap of
    [(nodemap,"")] -> do
      (TOD time _) <- getClockTime
      modifyMVar mvarMap (\x -> return $ (Gossip.merge x nodemap time, ()))
    _ -> putStrLn "Failed to parse message."
  hClose handle
  putStrLn $ "\"" ++ gotmap ++ "\""

  
getConfigOrFail :: IO (ConfigParser)
getConfigOrFail = do
  config <- getConfig "Gossip.conf"
  case config of 
    Nothing -> error "Couldn't get config."
    Just conf -> return conf

initialize :: ConfigParser -> Time -> Map ID Node
initialize conf time =
    let ip = getCrucial conf "gossip" "bindip" in
    updateMe Map.empty ip time time


doUpdate :: HostName -> Time -> MVar (Map ID Node) -> IO()
doUpdate host join mvarMap = do
  (TOD now _) <- getClockTime
  modifyMVar mvarMap (\x -> return $ (updateMe x host join now, ()))
  modifyMVar mvarMap (\x -> return $ (markFailed x now, ()))
  modifyMVar mvarMap (\x -> do
                         putStrLn $ show x
                         return (x, ()))
  return ()
  
getCrucial :: (Get_C a) => ConfigParser -> String -> String -> a
getCrucial cp section key = 
    let val = (getValue cp section key) in
    case val of
      Left err -> error $ "Failed to get value from config: " ++ err
      Right v -> v
