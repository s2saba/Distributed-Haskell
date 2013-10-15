module Distributed.Gossip.Data (
  Node (Node),
  Status (..),
  ID (..),
  Time,
  gossipMerge,
  updateNode,
  markFailed,
  findLiveAnytime,
  prettyMemberList,
  filterDead
  ) where

import Distributed.Config
import Network
import Data.Word
import Data.Map as Map
import Data.ConfigFile


type Time = Integer

-- An ID used to map a host, port, and join time to its node
data ID = ID {host :: HostName,
              port :: Word16,
              join :: Time} deriving (Eq, Ord, Show, Read)

data Status = Dead | Alive deriving (Show, Read, Eq, Ord)

-- The Node type, tracks status of a node in the cluster.
data Node = Node { hostName :: HostName,
                   hostPort :: Word16,
                   joinTime :: Time,
                   heartBeats :: Integer,
                   updateTime :: Time,
                   getStatus :: Status } deriving (Show, Read)

-- One node is equal to another when the node is communicating on the same
-- hostname, port, and joined at the same time.
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

-- Merge two member maps. The results will contain any entries from the new map not in the current map,
-- and updated nodes if the new map contains more recent heartbeats than we have in the current map.
gossipMerge :: Map ID Node -> Map ID Node -> Time -> Map ID Node
gossipMerge current new now = Map.fold (newest now) (difference current new) new
  where newest newTime node@(Node host port join heartbeat time status) acc =
          case Map.lookup (getID node) current of
            Nothing -> insert (getID node) (Node host port join heartbeat newTime status) acc
            Just currentNode@(Node _ _ _ currheartbeat currtime currstatus) ->
              if heartBeats currentNode < heartBeats node
              then insert (getID node) (Node host port join heartbeat newTime status) acc
              else insert (getID currentNode) (Node host port join currheartbeat currtime currstatus) acc

-- Update a particular node's updateTime.
updateNode :: Map ID Node -> ID -> Time -> Map ID Node
updateNode memmap id@(ID host port time) nowTime =
    case Map.lookup id memmap of
      Nothing -> insert id (Node host port time 1 nowTime Alive) memmap
      Just (Node _ _ _ heartbeats _ _) ->
        insert id (Node host port time (heartbeats + 1) nowTime Alive) memmap

-- Return a new map with all the nodes that haven't produced heartbeats within tFail seconds marked dead,
-- And all the nodes that haven't produced heartbeats within tFail * 2 seconds removed.
markFailed :: Map ID Node -> Time -> Time -> Map ID Node
markFailed memmap nowTime tFail = Map.mapMaybe setFailed memmap
  where setFailed (Node host port join heartBeat time status)
          | (time + (tFail * 2)) <= nowTime = Nothing
          | (time + tFail) < nowTime = Just (Node host port join heartBeat time Dead)
          | otherwise = Just (Node host port join heartBeat time status)

-- Maybe return a node representing a particular host and port, regardless of when it joined.
-- This is useful for getting information about ourselves when we rely on a contact to add us
-- to the global membership list, or when we want to check if the contact is contained in our
-- membership map.
findLiveAnytime :: Map ID Node -> ID -> Maybe Node
findLiveAnytime members (ID idHost idPort _) = Map.foldl findAlive Nothing members
  where findAlive acc node@(Node host port _ _ _ status) =
          if idHost == host && idPort == port && status == Alive then
            Just node
          else
            acc

-- Pretty print the membership list for human consumption
prettyMemberList :: Map ID Node -> [String]
prettyMemberList members = Prelude.map (\x -> show (hostName x, hostPort x, joinTime x, heartBeats x, getStatus x)) $ elems members

-- Return a map containing only the living nodes in our map.
filterDead :: Map ID Node -> Map ID Node
filterDead = Map.filter (\x -> getStatus x == Alive)

-- Create an ID for a node.
getID :: Node -> ID
getID node = ID (hostName node) (hostPort node) (joinTime node)
