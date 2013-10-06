module Gossip where

import Network
import System.IO
import System.Time
import Data.Map as Map

rendezvous = "192.168.1.200"

defaultPort = PortNumber 8080

type ID = String
type HeartbeatCount = Int
type Time = Integer

data Status = Dead | Alive deriving (Show, Read, Eq, Ord)

data Node = Node { getHostName :: HostName,
                   getHeartBeats :: HeartbeatCount,
                   getTime :: Time,
                   getStatus :: Status } deriving (Show, Read)

instance Eq Node where
  (Node hosta _ timea _) == (Node hostb _ timeb _) = (hosta == hostb) && (timea == timeb)
  
instance Ord Node where
  (Node hosta _ timea _) < (Node hostb _ timeb _)
    | hosta == hostb = timea < timeb
    | otherwise = hosta < hostb
  (Node hosta _ timea _) > (Node hostb _ timeb _)
    | hosta == hostb = timea > timeb
    | otherwise = hosta > hostb
  nodea >= nodeb = (nodea > nodeb) || (nodea == nodeb)
  nodea <= nodeb = (nodea < nodeb) || (nodea == nodeb)

merge :: Map ID Node -> Map ID Node -> Map ID Node
merge current new = Map.fold newest (difference current new) new
  where newest node@(Node host heartbeat time status) acc =
          case (Map.lookup (getID node) current) of
            Nothing -> insert (getHostName node) node acc
            Just currentNode -> if (getHeartBeats currentNode) < (getHeartBeats node)
                                then insert (getID node) node acc
                                else insert (getID currentNode) currentNode acc

getID :: Node -> ID
getID node = makeID (getHostName node) (show $ getTime node)

makeID :: HostName -> Time -> ID
makeID hostname time = hostname ++ "|" ++ time

sendList :: Map ID Node -> Node -> IO()
sendList map node = sendTo (getHostName node) defaultPort (show $ toList set)

updateMe :: Map ID Node -> HostName -> Time -> Map ID Node
updateMe map hostname joinTime =
  let identifier = makeID hostname joinTime in
  case Map.lookup identifier map of
    Nothing -> 



xNode = Node "192.168.1.200" 1 123 Alive
yNode = Node "192.168.1.200" 2 123 Alive
zNode = Node "192.168.1.201" 1 156 Alive

x = singleton (getID xNode) xNode
y = singleton (getID yNode) yNode
z = singleton (getID zNode) zNode


