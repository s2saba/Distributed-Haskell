module Distributed.Gossip where

import Network
import System.IO
import Data.Map as Map

type ID = String
type Time = Integer

data Status = Dead | Alive deriving (Show, Read, Eq, Ord)

data Node = Node { hostName :: HostName,
                   joinTime :: Time,
                   heartBeats :: Integer,
                   updateTime :: Time,
                   getStatus :: Status } deriving (Show, Read)

instance Eq Node where
  (Node hosta joina _ _ _) == (Node hostb joinb _ _ _) = (hosta == hostb) && (joina == joinb)
  
instance Ord Node where
  (Node hosta joina _ _ _) < (Node hostb joinb _ _ _)
    | hosta == hostb = joina < joinb
    | otherwise = hosta < hostb
  (Node hosta joina _ _ _) > (Node hostb joinb _ _ _)
    | hosta == hostb = joina > joinb
    | otherwise = hosta > hostb
  nodea >= nodeb = (nodea > nodeb) || (nodea == nodeb)
  nodea <= nodeb = (nodea < nodeb) || (nodea == nodeb)

merge :: Map ID Node -> Map ID Node -> Time -> Map ID Node
merge current new time = Map.fold (newest time) (difference current new) new
  where newest newTime node@(Node host join heartbeat time status) acc =
          case (Map.lookup (getID node) current) of
            Nothing -> insert (getID node) (Node host join heartbeat newTime status) acc
            Just currentNode@(Node _ _ currheartbeat currtime currstatus) ->
              if (heartBeats currentNode) < (heartBeats node)
              then insert (getID node) (Node host join heartbeat newTime status) acc
              else insert (getID currentNode) (Node host join currheartbeat currtime currstatus) acc

updateTimes :: Time -> Map ID Node -> Map ID Node
updateTimes time members = Map.map (\(Node host join hb last status) -> Node host join hb time status) members

getID :: Node -> ID
getID node = makeID (hostName node) (joinTime node)

makeID :: HostName -> Time -> ID
makeID hostname time = hostname ++ "|" ++ (show time)

sendList :: Map ID Node -> Node -> PortID -> IO()
sendList memmap node port = sendTo (hostName node) port (show $ toList memmap)

updateMe :: Map ID Node -> HostName -> Time -> Time -> Map ID Node
updateMe memmap hostname time nowTime =
    let identifier = makeID hostname time in
    case (Map.lookup identifier memmap) of
      Nothing -> insert identifier (Node hostname time 1 nowTime Alive) memmap
      Just (Node _ _ heartbeats _ _) ->
        insert identifier (Node hostname time (heartbeats + 1) nowTime Alive) memmap

markFailed :: Map ID Node -> Time -> Map ID Node
markFailed memmap nowTime = Map.mapMaybe setFailed memmap
  where setFailed (Node host join heartBeat time status)
          | (time + 10) < nowTime = Nothing
          | (time + 5) < nowTime = Just (Node host join heartBeat time Dead)
          | otherwise = Just (Node host join heartBeat time status)
                   
                   
xNode = Node "192.168.1.200" 1 100 12 Alive
yNode = Node "192.168.1.200" 2 123 13 Alive
zNode = Node "192.168.1.201" 1 156 31 Alive

x = singleton (getID xNode) xNode
y = singleton (getID yNode) yNode
z = singleton (getID zNode) zNode


