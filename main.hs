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
import System.Random
import Control.Monad.State
import Control.Exception

main = runGossip

runGossip = do
  (TOD time _) <- getClockTime
  config <- getConfigOrFail
  let membermap = initialize config time
  let ip = getCrucial config "gossip" "bindip"
  let port = PortNumber (PortNum $ getCrucial config "gossip" "contactport")
  mapmvar <- newMVar membermap
  x <- newEmptyMVar
  listener <- forkIO $ listenForGossip mapmvar x $ port
  connectToContact mapmvar config port
  tmr <- repeatedTimer (doUpdate ip time port mapmvar) $ sDelay 1
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
  
getConfigOrFail :: IO (ConfigParser)
getConfigOrFail = do
  config <- getConfig "Gossip.conf"
  case config of 
    Nothing -> error "Couldn't get config."
    Just conf -> return conf

initialize :: ConfigParser -> Time -> Map ID Node
initialize conf time =
  let val = getValue conf "gossip" "contactip" :: Either String String in 
  case val of
    Right host ->
      let ip = getCrucial conf "gossip" "bindip" in
      updateMe Map.empty ip time time
    Left err ->
      Map.empty


doUpdate :: HostName -> Time -> PortID -> MVar (Map ID Node) -> IO()
doUpdate host join port mvarMap = do
  (TOD now _) <- getClockTime
  memmap <- readMVar mvarMap
  modifyMVar mvarMap (\x -> return $ (updateMe x host join now, ()))
  modifyMVar mvarMap (\x -> return $ (markFailed x now, ()))
  modifyMVar mvarMap (\x -> do
                         let hostmap = Prelude.map (\y ->
                                                     ((hostName y), (getStatus y))) $ elems x
                         putStrLn $ show hostmap
                         return (x, ()))
  sendGossip host join memmap port
  return ()
  
getCrucial :: (Get_C a) => ConfigParser -> String -> String -> a
getCrucial cp section key = 
    let val = (getValue cp section key) in
    case val of
      Left err -> error $ "Failed to get value from config: " ++ err
      Right v -> v

connectToContact :: MVar (Map ID Node) -> ConfigParser -> PortID -> IO()
connectToContact mapMVar config port = do
  memmap <- readMVar mapMVar
  if memmap == Map.empty then
    let val = getValue config "gossip" "contactip" in
    case val of
      Left err -> return ()
      Right host -> do
        try $ Network.sendTo host port "Join" :: IO(Either SomeException ())
        connectToContact mapMVar config port
  else
   return ()

sendGossip :: HostName -> Time -> Map ID Node -> PortID -> IO ()
sendGossip myhost join memmap port = do
  let othermem = Map.delete (makeID myhost join) memmap 
      hosts = Prelude.map (\x -> hostName x) $ elems othermem
      count = length hosts
  x <- sequence $ replicate (ceiling $ (fromIntegral count) / 2) $ randomRIO (0, (count - 1))
  putStrLn $ show x
  sequence $ Prelude.map (\y -> try $ Network.sendTo (hosts !! y) port $ show memmap :: IO (Either SomeException ())) x
  putStrLn $ "Sent to hosts" ++ (show $ Prelude.map (\y -> hosts !! y) x)
