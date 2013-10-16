import Distributed.Gossip.IO as Gossip
import Distributed.Config
import Control.Concurrent
import Control.Concurrent.Timer          --repeatedTimer
import Control.Concurrent.Suspend.Lifted -- sDelay

-- Simple gossip example. We wait on the listener to die before exiting.
main = do
  putStrLn "Running a 60 second sample."
  gossip <- runGossip "DHaskell.conf"
  oneShotTimer (stopGossip gossip) $ sDelay 60
  tmr <- repeatedTimer (getMembers gossip >>= putStrLn . show ) $ sDelay 1
  waitGossip gossip
  stopTimer tmr
  putStrLn "Gossip is ended."
  return ()
