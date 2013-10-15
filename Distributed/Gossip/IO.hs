module Distributed.Gossip.IO (
  runGossip,
  stopGossip
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
import Data.Map as Map
import Control.Monad (when, replicateM)
import Control.Arrow ((&&&))

-- A function that's run periodically. It updates our node (if we're in the membership map),
-- marks the failed nodes, and sends out gossip.
-- It also tries to join the designated contact node if there is one in the case that:
--       1) Our node isn't in the membership map
--       2) The contact's node isn't in the membership map
doUpdate :: Maybe ID -> Time -> MVar ID -> String -> MVar (Map ID Node) -> IO()
doUpdate contact tFail myIdMVar v6interface mvarMap = do
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
  -- Print membership
  putStrLn $ "Members: " ++ show now ++ " | " ++ show (prettyMemberList memberMap)

-- Multicast our membership map in gossip style. We only broadcast to nodes that aren't us,
-- and we only send nodes marked alive.
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

-- Send a join message to the contact
sendJoin :: ID -> ID -> PortID -> IO ()
sendJoin (ID host port _) myId (PortNumber myPort) = trySend host (portFromWord port) myId $ "Join " ++ show myPort

-- Begin listening for gossip. On unhandled exception, set the MVar so someone who cares can
-- learn about the failure.
-- (This should probably be implemented better. I'd like to put an exception in the MVar.)
listenForGossip :: MVar Bool -> MVar ID -> String -> MVar (Map ID Node) -> IO ()
listenForGossip alive myIDMVar v6interface memberMVar = do
  (ID host port _) <- readMVar myIDMVar
  sock <- listenSocket (host ++ v6interface) (portFromWord port)
  putStrLn $ "Listening on " ++ host ++ v6interface ++ ":" ++ show port
  waitForConnect sock myIDMVar memberMVar
  putMVar alive False

-- Wait for and accept a connection, then fork the handler.
waitForConnect :: Socket -> MVar ID -> MVar (Map ID Node) -> IO ()
waitForConnect sock myIDMVar mvarMap = do
  connection <- acceptSocket sock
  forkIO $ handleConnection connection myIDMVar mvarMap
  waitForConnect sock myIDMVar mvarMap

-- Handle a connection.
-- The message is either a join, in which case we add the node to the membership map,
-- or it's a membership map, and we merge our map with the received one.
-- We also update our id MVar with the latest node representing us in the map.
handleConnection :: (Handle, HostName, PortNumber) -> MVar ID -> MVar (Map ID Node) -> IO ()
handleConnection (handle, host, port) myIDMVar memberMVar = do
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
        _ -> putStrLn $ "Failed to parse message from " ++ host ++ ":" ++ show port

  hClose handle
  
-- A function that accepts a config file path, reads the configuration, and begins gossiping
-- based on that configuration. It currently returns the ThreadID of the listener, the
-- TimerIO of the periodic update function, and the MVar that will be filled if the
-- listener stops for some reason.
-- TODO: If the listener stops, halt other portions of the gossip protocol
runGossip :: FilePath -> IO (ThreadId, TimerIO, MVar Bool)
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
      
  (contact, myIdMVar) <-
    case (contactIP, contactPort) of
      (Just ip, Just port) -> do
        putStrLn "This node is NOT the contact node."
        let contact = Just (ID ip port 0)
        myIdMVar <- newMVar (ID bindIP bindPort 0)
        return (contact, myIdMVar)
      (_, _) -> do
        putStrLn "This node IS the contact node."
        (TOD now _) <- getClockTime 
        myIdMVar <- newMVar (ID bindIP bindPort now)
        return (Nothing, myIdMVar)

      
  memberMVar <- newMVar Map.empty
  alive <- newEmptyMVar

  listener <- forkIO $ listenForGossip alive myIdMVar ipv6Interface memberMVar
  tmr <- repeatedTimer (doUpdate contact tFail myIdMVar ipv6Interface memberMVar) $ sDelay tGossip

  return (listener, tmr, alive)

-- A function to stop the gossip protocol.
stopGossip :: (ThreadId, TimerIO, MVar Bool) -> IO ()
stopGossip (listen, timer, _) = do
  killThread listen
  stopTimer timer
