import Distributed.Gossip.IO as Gossip
import Distributed.Config
import Control.Concurrent

-- Simple gossip example. We wait on the listener to die before exiting.
main = do
  (listen, timer, alive) <- runGossip "Gossip.conf"
  takeMVar alive
  return ()
