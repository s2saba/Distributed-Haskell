module Distributed.Gossip.IO (
  runGossip,
  waitGossip,
  getMembers,
  stopGossip,
  Gossip
  )  where

import Distributed.Gossip.Data
import Distributed.Config
import Distributed.Network
import Network

import System.IO                         -- Handles, etc
import System.Time                       -- getClockTime
import System.Random                     -- randomRIO
import Control.Concurrent                -- Mvars
import Data.List (nub, stripPrefix)      -- nub, stripPrefix
import Data.String.Utils                 -- strip
import Control.Concurrent.Timer          --repeatedTimer
import Control.Concurrent.Suspend.Lifted -- sDelay
import Control.Exception                 -- try
import Data.Map as Map
import Control.Monad (when, replicateM)
import Control.Arrow ((&&&))

-- | The Gossip data structure. 
data Gossip = Gossip { listener :: ThreadId,
                       updater :: TimerIO,
                       flusher :: TimerIO,
                       log :: (MVar (Maybe Handle)),
                       alive :: (MVar Bool),
                       memberMap :: (MVar (Map ID Node))}

-- | A function that's run periodically. It updates our node (if we're in the membership map),
-- | marks the failed nodes, and sends out gossip.
-- | It also tries to join the designated contact node if there is one in the case that:
-- |       1) Our node isn't in the membership map
-- |       2) The contact's node isn't in the membership map
doUpdate :: Maybe ID -> Time -> MVar ID -> String -> MVar (Maybe Handle) -> MVar (Map ID Node) -> IO()
doUpdate contact tFail myIdMVar v6interface logMVar mvarMap = do
  (TOD now _) <- getClockTime
  id@(ID myHost myPort myTime) <- readMVar myIdMVar

  let idInterface = ID (myHost ++ v6interface) myPort myTime

  -- Heartbeat our own list
  when (myTime /= 0) (modifyMVar mvarMap (\x -> return (updateNode x id now, ())))

  -- Mark dead nodes
  modifyMVar mvarMap (\x -> return (markFailed x now tFail, ()))      

  memberMap <- readMVar mvarMap
  case contact of
    Nothing -> return ()
    Just contactId -> do
       case findLiveAnytime memberMap contactId of
         Nothing -> sendJoin contactId idInterface $ portFromWord myPort
         Just node -> return ()
       case findLiveAnytime memberMap id of
         Nothing -> sendJoin contactId idInterface $ portFromWord myPort
         Just node -> return ()

  id <- readMVar myIdMVar
  sendGossip memberMap id v6interface
  -- Log Membership
  writeLog logMVar $ "Members: " ++ show now ++ " | " ++ show (prettyMemberList memberMap)

-- | Multicast our membership map in gossip style. We only broadcast to nodes that aren't us,
-- | and we only send nodes marked alive.
sendGossip :: Map ID Node -> ID -> String -> IO()
sendGossip members myId@(ID myHost myPort myTime) v6interface = do
  let myV6Id = ID (myHost ++ v6interface) myPort myTime
      otherMembers = Map.delete myId members
      notDead = filterDead members
      hosts = Prelude.map (host &&& port) $ keys otherMembers
      count = length hosts
      numToSend = ceiling $ fromIntegral count / 2

  chosenHosts <- replicateM numToSend $ randomRIO (0, count - 1)
  mapM_ (\y -> let host = fst $ hosts !! y
                   port = portFromWord $ snd $ hosts !! y in
              forkIO $ trySend host port myV6Id $ show notDead) $ nub chosenHosts
  return ()

-- | Send a join message to the contact
sendJoin :: ID -> ID -> PortID -> IO ()
sendJoin (ID host port _) myId (PortNumber myPort) = trySend host (portFromWord port) myId $ "Join " ++ show myPort

-- | Begin listening for gossip. On unhandled exception, set the MVar so someone who cares can
-- | learn about the failure.
-- | (This should probably be implemented better. I'd like to put an exception in the MVar.)
listenForGossip :: MVar Bool -> MVar ID -> String -> MVar (Maybe Handle) -> MVar (Map ID Node) -> IO ()
listenForGossip alive myIDMVar v6interface logMVar memberMVar = do
  (ID host port _) <- readMVar myIDMVar
  sock <- listenSocket (host ++ v6interface) (portFromWord port)
  writeLog logMVar $ "Listening on " ++ host ++ v6interface ++ ":" ++ show port
  try $ waitForConnect sock myIDMVar logMVar memberMVar :: IO (Either SomeException ())
  writeLog logMVar $ "Ending listening. Closing port."
  sClose sock
  putMVar alive False

-- | Wait for and accept a connection, then fork the handler.
waitForConnect :: Socket -> MVar ID -> MVar (Maybe Handle) -> MVar (Map ID Node) -> IO ()
waitForConnect  sock myIDMVar logMVar mvarMap = do
  connection <- acceptSocket sock
  forkIO $ handleConnection connection myIDMVar logMVar mvarMap
  waitForConnect sock myIDMVar logMVar mvarMap

-- | Handle a connection.
-- | The message is either a join, in which case we add the node to the membership map,
-- | or it's a membership map, and we merge our map with the received one.
-- | We also update our id MVar with the latest node representing us in the map.
handleConnection :: (Handle, HostName, PortNumber) -> MVar ID -> MVar (Maybe Handle) -> MVar (Map ID Node) -> IO ()
handleConnection (handle, host, port) myIDMVar logMVar memberMVar = do
  message <- hGetContents handle
  (TOD time _) <- getClockTime
  let strippedmsg = strip message
  case stripPrefix "Join " strippedmsg of
    Just portStr ->
      modifyMVar memberMVar (\x -> return (updateNode x (ID host (fromIntegral $ read portStr) time) time, ()))
    Nothing -> 
      case reads strippedmsg of
        [(nodemap,"")] -> do
          modifyMVar memberMVar (\x -> return (gossipMerge x nodemap time, ()))
          memberMap <- readMVar memberMVar
          myId <- readMVar myIDMVar
          let myNode = findLiveAnytime memberMap myId
          case myNode of
            Nothing -> return ()
            Just (Node host port time _ _ _) -> modifyMVar myIDMVar (\x -> return (ID host port time, ()))
        _ -> writeLog logMVar  $ "Failed to parse message from " ++ host ++ ":" ++ show port

  hClose handle
  
-- | A function that accepts a config file path, reads the configuration, and begins gossiping
-- | based on that configuration. It currently returns the ThreadID of the listener, the
-- | TimerIO of the periodic update function, and the MVar that will be filled if the
-- | listener stops for some reason.
-- | TODO: If the listener stops, halt other portions of the gossip protocol
runGossip :: FilePath -> IO Gossip
runGossip filePath = do
  config <- getConfigOrFail filePath
  let contactPort = configGetValue config "gossip" "contactport"
      contactIP = configGetValue config "gossip" "contactip"
      ipv6Interface = case configGetValue config "gossip" "ipv6_interface" of
        Nothing -> ""
        Just interface -> '%':interface
      bindIP = configGetCrucial config "gossip" "bindip"
      bindPort = configGetCrucial config "gossip" "bindport"
      tFail = configGetCrucial config "gossip" "tfail"
      tGossip = configGetCrucial config "gossip" "tgossip"
      logFile = configGetValue config "gossip" "log_file"
      tflush = case configGetValue config "gossip" "tflush_log" of
        Nothing -> 3
        Just seconds -> seconds
      noBuffer = case configGetValue config "gossip" "no_buffer_log" of
        Nothing -> False
        Just bool -> bool
  
  logMVar <- case logFile of
    Nothing -> newMVar Nothing
    Just filepath -> if noBuffer
                     then (openFile filepath AppendMode) >>=
                          (\x -> do hSetBuffering x NoBuffering; return x) >>=
                          newMVar . Just
                     else (openFile filepath AppendMode) >>=
                          newMVar . Just 

  
  (contact, myIdMVar) <-
    case (contactIP, contactPort) of
      (Just ip, Just port) -> do
        writeLog logMVar "This node is NOT the contact node."
        let contact = Just (ID ip port 0)
        myIdMVar <- newMVar (ID bindIP bindPort 0)
        return (contact, myIdMVar)
      (_, _) -> do
        writeLog logMVar "This node IS the contact node."
        (TOD now _) <- getClockTime 
        myIdMVar <- newMVar (ID bindIP bindPort now)
        return (Nothing, myIdMVar)

      
  memberMVar <- newMVar Map.empty
  alive <- newEmptyMVar

  listener <- forkIO $ listenForGossip alive myIdMVar ipv6Interface logMVar memberMVar
  tmr <- repeatedTimer (doUpdate contact tFail myIdMVar ipv6Interface logMVar memberMVar) $ sDelay tGossip
  flusher <- repeatedTimer (modifyMVar logMVar flushLog) $ sDelay tflush

  putStrLn "Returning new gossip!"
  return $ Gossip listener tmr flusher logMVar alive memberMVar
                       

-- | A function to stop the gossip protocol.
stopGossip :: Gossip -> IO ()
stopGossip (Gossip listen timer flusher log alive members) = do
  killThread listen
  stopTimer timer
  stopTimer flusher
  takeMVar alive
  modifyMVar log (\l -> case l of
                     Nothing -> return (Nothing, ())
                     Just handle -> do
                       hFlush handle
                       hClose handle
                       return (Nothing,()))
  modifyMVar members (\_ -> return (Map.empty, ()))
  putMVar alive False
  return ()

-- | Return a list of hostnames that are currently alive in the cluster
getMembers :: Gossip -> IO [HostName]
getMembers (Gossip _ _ _ _ _ members) = do
  membermap <- readMVar members
  return $ Prelude.map (\(ID host _ _) -> host) $ keys $ filterDead membermap

-- | Wait for gossip to die (something to stop it)
waitGossip :: Gossip -> IO ()
waitGossip (Gossip _ _ _ _ alive _) = do
  takeMVar alive
  putMVar alive False
  return ()

-- | A function that allows asynchronous writes to a log file.
writeLog :: MVar (Maybe Handle) -> String -> IO ()
writeLog logMVar text = do
  modifyMVar logMVar (\mHandle -> case mHandle of
                         Nothing -> return (mHandle, ())
                         Just handle ->  do hPutStrLn handle text
                                            return (mHandle, ()))


flushLog :: Maybe Handle -> IO (Maybe Handle,())
flushLog Nothing = return (Nothing,())
flushLog (Just handle) = do
                          hFlush handle
                          return (Just handle, ())
