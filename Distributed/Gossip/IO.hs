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


doUpdate :: Maybe ID -> Time -> MVar ID -> String -> MVar (Map ID Node) -> IO()
doUpdate contact tFail myIdMVar v6interface mvarMap = do
  (TOD now _) <- getClockTime
  id@(ID myHost myPort myTime) <- readMVar myIdMVar

  let idInterface = (ID (myHost ++ v6interface) myPort myTime)
  
  if myTime /= 0 then
    modifyMVar mvarMap (\x -> return $ (updateNode x id now, ())) -- Heartbeat our own list
  else
    return ()

  modifyMVar mvarMap (\x -> return $ (markFailed x now tFail, ()))      -- Mark dead nodes

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
  putStrLn $ "Members: " ++ (show now) ++ " | " ++ (show $ prettyMemberList memberMap)                    -- Print membership

sendGossip :: Map ID Node -> ID -> String -> IO()
sendGossip members myId@(ID myHost myPort myTime) v6interface = do
  let myV6Id = (ID (myHost ++ v6interface) myPort myTime)
      otherMembers = Map.delete myId members
      notDead = filterDead members
      hosts = Prelude.map (\x -> (host x, port x)) $ keys otherMembers
      count = length hosts
      numToSend = ceiling $ (fromIntegral count) / 2

  chosenHosts <- sequence $ replicate numToSend $ randomRIO (0, (count - 1))
  sequence $ Prelude.map (\y -> let host = fst $ hosts !! y
                                    port = portFromWord $ snd $ hosts !! y in
                                forkIO $ trySend host port myV6Id $ show notDead) $ nub chosenHosts
  return ()

sendJoin :: ID -> ID -> PortID -> IO ()
sendJoin (ID host port _) myId (PortNumber myPort) = trySend host (portFromWord port) myId $ "Join " ++ (show myPort)


listenForGossip :: MVar Bool -> MVar ID -> String -> MVar (Map ID Node) -> IO ()
listenForGossip alive myIDMVar v6interface memberMVar = do
  (ID host port _) <- readMVar myIDMVar
  sock <- listenSocket (host ++ v6interface) (portFromWord port)
  putStrLn $ "Listening on " ++ host ++ v6interface ++ ":" ++ (show port)
  waitForConnect sock myIDMVar memberMVar
  putMVar alive False

  
waitForConnect :: Socket -> MVar ID -> MVar (Map ID Node) -> IO ()
waitForConnect sock myIDMVar mvarMap = do
  connection <- acceptSocket sock
  forkIO $ handleConnection connection myIDMVar mvarMap
  waitForConnect sock myIDMVar mvarMap

handleConnection :: (Handle, HostName, PortNumber) -> MVar ID -> MVar (Map ID Node) -> IO ()
handleConnection (handle, host, port) myIDMVar memberMVar = do
  message <- hGetContents handle
  (TOD time _) <- getClockTime
  let strippedmsg = strip message
  case stripPrefix "Join " strippedmsg of
    Just portStr ->
      modifyMVar memberMVar (\x -> return $ (updateNode x (ID host (fromIntegral $ read portStr) time) time, ()))
    Nothing -> 
      case reads $ strippedmsg of
        [(nodemap,"")] -> do
          modifyMVar memberMVar (\x -> return $ (gossipMerge x nodemap time, ()))
          memberMap <- readMVar memberMVar
          myId <- readMVar myIDMVar
          let myNode = findLiveAnytime memberMap myId
          case myNode of
            Nothing -> return ()
            Just (Node host port time _ _ _) -> modifyMVar myIDMVar (\x -> return $ ((ID host port time), ()))
        _ -> do
          putStrLn $ "Failed to parse message from " ++ host ++ ":" ++ (show port)

  hClose handle
  

runGossip :: FilePath -> IO (ThreadId, TimerIO, MVar Bool)
runGossip filePath = do
  config <- getConfigOrFail filePath
  let contactPort = configGetValue config "gossip" "contactport"
      contactIP = configGetValue config "gossip" "contactip"
      ipv6Interface = case configGetValue config "gossip" "ipv6_interface" of
        Nothing -> ""
        Just interface -> "%" ++ interface
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

stopGossip :: (ThreadId, TimerIO, MVar Bool) -> IO ()
stopGossip (listen, timer, _) = do
  killThread listen
  stopTimer timer
