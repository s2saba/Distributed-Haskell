
import System.IO
import Network

main = do
  socket <- listenOn $ PortNumber 8080
  (handle, _, _) <- accept socket
  contents <- hGetContents handle
  hPutStr handle contents
