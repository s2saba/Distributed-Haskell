module Distributed.Network (
  listenSocket,
  acceptSocket,
  portFromWord,
  trySend
  )  where

import Network                                            -- PortID
import Network.Socket                                     -- (Obviously)
import System.IO                                          -- Handles
import Network.BSD (getProtocolNumber, ProtocolName)
import Data.Word                                          -- Word16
import Control.Exception                                  -- try
import Distributed.Gossip.Data

-- | Create a listenign socket that's bound to a particular hostname and port.
listenSocket :: HostName -> PortID -> IO Socket
listenSocket host (PortNumber port) = do
  infos <- getAddrInfo Nothing (Just host) Nothing
  let info = head infos
      sockAddr = case addrAddress info of
        (SockAddrInet _ addr) -> SockAddrInet port addr
        (SockAddrInet6 _ _ addr scope) -> SockAddrInet6 port 0 addr scope
        
  proto <- getProtocolNumber "tcp"
  sock <- socket (addrFamily info) Stream proto
  setSocketOption sock ReuseAddr 1
  bindSocket sock sockAddr
  listen sock maxListenQueue
  return sock

-- | When we accept a connection, create an I/O handle along with hostname and port
-- | for the connected "client"
acceptSocket :: Socket -> IO (Handle, HostName, PortNumber)
acceptSocket sock = do
  (acceptedSocket, address) <- Network.Socket.accept sock
  let (sockAddr, port) = case address of
        (SockAddrInet port addr) -> (SockAddrInet port addr, port)
        (SockAddrInet6 port _ addr _) -> (SockAddrInet6 port 0 addr 0, port)
        
  (Just host, Nothing) <- getNameInfo [NI_NUMERICHOST] True False sockAddr
  handle <- socketToHandle acceptedSocket ReadMode 
  return (handle, host, port)

-- | Commonly used since we store ports as Word16 in Node and ID types.
portFromWord :: Word16 -> PortID
portFromWord word = PortNumber $ fromIntegral word

-- | Attempt to send a message to a host on a socket bound to a particular hostname and port.
-- | Fail silently. (Because Gossip doesn't care really if it can gossip to a node or not)
trySend :: HostName -> PortID -> ID -> String -> IO ()
trySend host port (ID myHost myPort _) string = do
  (try $ do
    localAddr <- sockAddrFromHostAndPort myHost $ portFromWord 0
    remoteAddr <- sockAddrFromHostAndPort host port
    socket <- socketFromHostProtocolAndType host "tcp" Stream
    bindSocket socket localAddr    
    connect socket remoteAddr
    send socket string
    Network.Socket.sClose socket) :: IO (Either SomeException ())
  return ()

-- | Internal function to set up bound sockets
socketFromHostProtocolAndType :: HostName -> ProtocolName -> SocketType -> IO Socket
socketFromHostProtocolAndType host protocol sockType = do
  infos <- getAddrInfo Nothing (Just host) Nothing
  let info = head infos
  proto <- getProtocolNumber protocol
  sock <- socket (addrFamily info) sockType proto
  setSocketOption sock ReuseAddr 1
  return sock

-- | Internal function to set up a sockAddr from a HostName and Port.
sockAddrFromHostAndPort :: HostName -> PortID -> IO SockAddr 
sockAddrFromHostAndPort host (PortNumber port) = do
  infos <- getAddrInfo Nothing (Just host) Nothing
  let info = head infos
      sockAddr = case addrAddress info of
        (SockAddrInet _ addr) -> SockAddrInet port addr
        (SockAddrInet6 _ _ addr scope) -> SockAddrInet6 port 0 addr scope
        
  return sockAddr  
