import Distributed.Gossip.IO as Gossip
import Distributed.Config
import Control.Concurrent

main = do
  (listen, timer, alive) <- runGossip "Gossip.conf"
  takeMVar alive
  return ()
