module Server
  ( server
  ) where

import Pipes
import Pipes.Network.TCP
import Pipes.Prelude qualified as P

import ServerEnv
import ServerM

server :: MonadIO m => Socket -> Effect (ServerM ServerEnv m) ()
server socket =
  do
    fromSocket socket bufferSize
    >-> P.tee P.print
    >-> P.map (const "HTTP/1.1 200 OK\r\n\r\n")
    >-> toSocket socket
