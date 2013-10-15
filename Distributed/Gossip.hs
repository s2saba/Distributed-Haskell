module Distributed.Gossip where

import Distributed.Config
import Network
import Network.Socket                    -- setSocketOption, ReuseAddr
import System.IO
import Data.Word
--import Data.List.Split
import Data.Map as Map
import Data.List (nub)                   -- nub
import System.Time                       -- getClockTime
import Control.Concurrent                -- Mvars
import System.Random                     -- randomRIO
import Control.Exception                 -- try
import Data.String.Utils                 -- strip
import Control.Concurrent.Timer          --repeatedTimer
import Control.Concurrent.Suspend.Lifted -- sDelay
import Data.ConfigFile


data ID = ID {host :: HostName,
              port :: Word16,
              join :: Time} deriving (Eq, Ord, Show, Read)

idToString :: ID -> String
idToString (ID host port join) = host ++ "|" ++ (show port) ++ "|" ++ (show join)
--instance Show ID where
--  show (ID host port join) = host ++ "|" ++ (show port) ++ "|" ++ (show join)

--instance Read ID where
--  read str = 

type Time = Integer

data Status = Dead | Alive deriving (Show, Read, Eq, Ord)

data Node = Node { hostName :: HostName,
                   hostPort :: Word16,
                   joinTime :: Time,
                   heartBeats :: Integer,
                   updateTime :: Time,
                   getStatus :: Status } deriving (Show, Read)

instance Eq Node where
  (Node hosta porta joina _ _ _) == (Node hostb portb joinb _ _ _) = (hosta == hostb) && (porta == portb) && (joina == joinb)
  
instance Ord Node where
  (Node hosta porta joina _ _ _) < (Node hostb portb joinb _ _ _)
    | hosta == hostb && porta == portb = joina < joinb
    | hosta == hostb = porta < portb
    | otherwise = hosta < hostb
  (Node hosta porta joina _ _ _) > (Node hostb portb joinb _ _ _)
    | hosta == hostb && porta == portb = joina > joinb
    | hosta == hostb = porta > portb
    | otherwise = hosta > hostb
  nodea >= nodeb = (nodea > nodeb) || (nodea == nodeb)
  nodea <= nodeb = (nodea < nodeb) || (nodea == nodeb)

gossipMerge :: Map ID Node -> Map ID Node -> Time -> Map ID Node
gossipMerge current new now = Map.fold (newest now) (difference current new) new
  where newest newTime node@(Node host port join heartbeat time status) acc =
          case (Map.lookup (getID node) current) of
            Nothing -> insert (getID node) (Node host port join heartbeat newTime status) acc
            Just currentNode@(Node _ _ _ currheartbeat currtime currstatus) ->
              if (heartBeats currentNode) < (heartBeats node)
              then insert (getID node) (Node host port join heartbeat newTime status) acc
              else insert (getID currentNode) (Node host port join currheartbeat currtime currstatus) acc

updateTimes :: Time -> Map ID Node -> Map ID Node
updateTimes time members = Map.map (\(Node host port join hb _ status) -> Node host port join hb time status) members

getID :: Node -> ID
getID node = ID (hostName node) (hostPort node) (joinTime node)

--sendList :: Map ID Node -> ID -> IO()
--sendList memmap (ID host port _) = Network.sendTo host (portFromWord port) (show $ toList memmap)

updateNode :: Map ID Node -> ID -> Time -> Map ID Node
updateNode memmap id@(ID host port time) nowTime =
    case (Map.lookup id memmap) of
      Nothing -> insert id (Node host port time 1 nowTime Alive) memmap
      Just (Node _ _ _ heartbeats _ _) ->
        insert id (Node host port time (heartbeats + 1) nowTime Alive) memmap

markFailed :: Map ID Node -> Time -> Time -> Map ID Node
markFailed memmap nowTime tFail = Map.mapMaybe setFailed memmap
  where setFailed (Node host port join heartBeat time status)
          | (time + (tFail * 2)) <= nowTime = Nothing
          | (time + tFail) < nowTime = Just (Node host port join heartBeat time Dead)
          | otherwise = Just (Node host port join heartBeat time status)

findLiveAnytime :: Map ID Node -> ID -> Maybe Node
findLiveAnytime members (ID idHost idPort _) = Map.foldl findAlive Nothing members
  where findAlive acc node@(Node host port _ _ _ status) =
          if idHost == host && idPort == port && status == Alive then
            Just node
          else
            acc
                                  
--joinNode :: Map ID Node -> ID -> Map ID Node
--joinNode memMap id@(ID host port join) = insert id (Node host port time 1 time Alive)

xNode = Node "192.168.1.200" 8080 1 100 12 Alive
yNode = Node "192.168.1.200" 8080 2 123 13 Alive
zNode = Node "192.168.1.201" 8080 1 156 31 Alive

x = singleton (getID xNode) xNode
y = singleton (getID yNode) yNode
z = singleton (getID zNode) zNode

prettyMemberList :: Map ID Node -> [String]
prettyMemberList members = Prelude.map (\x -> show $ (hostName x, hostPort x, joinTime x, getStatus x)) $ elems members

filterDead :: Map ID Node -> Map ID Node
filterDead members = Map.filter (\x -> (getStatus x) == Alive) members

--updateNode :: Map ID Node -> ID -> Time -> Map ID Node

doUpdate :: Maybe ID -> Time -> MVar ID -> MVar (Map ID Node) -> IO()
doUpdate contact tFail myIdMVar mvarMap = do
  (TOD now _) <- getClockTime
  myId <- readMVar myIdMVar
  modifyMVar mvarMap (\x -> return $ (updateNode x myId now, ())) -- Heartbeat our own list
  modifyMVar mvarMap (\x -> return $ (markFailed x now tFail, ()))      -- Mark dead nodes
  memberMap <- readMVar mvarMap
  case contact of
    Nothing -> return ()
    Just id -> case findLiveAnytime memberMap id of
      Nothing -> sendJoin id
      Just _ -> return ()
  sendGossip (Map.delete myId memberMap) myId
  putStrLn $ show $ prettyMemberList memberMap                    -- Print membership

sendGossip :: Map ID Node -> ID -> IO()
sendGossip members myId = do
  let otherMembers = Map.delete myId members
      notDead = filterDead members
      hosts = Prelude.map (\x -> (host x, port x)) $ keys otherMembers
      count = length hosts
      numToSend = ceiling $ (fromIntegral count) / 2

  chosenHosts <- sequence $ replicate numToSend $ randomRIO (0, (count - 1))
  sequence $ Prelude.map (\y -> let host = fst $ hosts !! y
                                    port = portFromWord $ snd $ hosts !! y in
                                forkIO $ trySend host port $ show notDead) $ nub chosenHosts
  return ()

sendJoin :: ID -> IO ()
sendJoin (ID host port _) = trySend host (portFromWord port) "Join"

portFromWord :: Word16 -> PortID
portFromWord word = PortNumber $ fromIntegral word


trySend :: HostName -> PortID -> String -> IO ()
trySend host port string = do
  try $ Network.sendTo host port string :: IO (Either SomeException ())
  return ()


listenForGossip :: MVar Bool -> MVar (Map ID Node) -> PortID -> IO ()
listenForGossip alive memberMVar listenPort = do
  sock <- listenOn listenPort
  putStrLn $ "Listening on port " ++ (show listenPort)
  setSocketOption sock ReuseAddr 1
  waitForConnect sock memberMVar
  putMVar alive False

  
waitForConnect :: Socket -> MVar (Map ID Node) -> IO ()
waitForConnect sock mvarMap = do
  connection <- Network.accept sock
  forkIO $ handleConnection connection mvarMap
  waitForConnect sock mvarMap

handleConnection :: (Handle, HostName, PortNumber) -> MVar (Map ID Node) -> IO ()
handleConnection (handle, host, port) memberMVar = do
  message <- hGetContents handle
  (TOD time _) <- getClockTime
  let strippedmsg = strip message
  if strippedmsg == "Join" then
    modifyMVar memberMVar (\x -> return $ (updateNode x (ID host (fromIntegral port) time) time, ()))
  else
    case reads $ strippedmsg of
      [(nodemap,"")] -> do
        modifyMVar memberMVar (\x -> return $ (gossipMerge x nodemap time, ()))
      _ -> do
        putStrLn $ "Failed to parse message from " ++ host ++ ":" ++ (show port)

  hClose handle
  

runGossip :: FilePath -> IO (ThreadId, TimerIO, MVar Bool)
runGossip filePath = do
  config <- getConfigOrFail filePath
  let contactPort = configGetValue config "gossip" "contactport"
      contactIP = configGetValue config "gossip" "contactip"
      bindIP = configGetCrucial config "gossip" "bindip"
      bindPort = configGetCrucial config "gossip" "bindport"
      tFail = configGetCrucial config "gossip" "tfail"

  (contact, myIdMVar) <-
    case (contactIP, contactPort) of
      (Just ip, Just port) -> do
        let contact = Just (ID ip port 0)
        myIdMVar <- newEmptyMVar
        return (contact, myIdMVar)
      (_, _) -> do
        (TOD now _) <- getClockTime 
        myIdMVar <- newMVar (ID bindIP bindPort now)
        return (Nothing, myIdMVar)

      
  memberMVar <- newMVar Map.empty
  alive <- newEmptyMVar

  listener <- forkIO $ listenForGossip alive memberMVar $ PortNumber $ fromIntegral bindPort
  tmr <- repeatedTimer (doUpdate contact tFail myIdMVar memberMVar) $ sDelay 1
  return (listener, tmr, alive)

stopGossip :: (ThreadId, TimerIO, MVar Bool) -> IO ()
stopGossip (listen, timer, _) = do
  killThread listen
  stopTimer timer
