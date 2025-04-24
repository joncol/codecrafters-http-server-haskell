module Server
  ( server
  ) where

import Pipes
import Pipes.Attoparsec qualified as A
import Pipes.Network.TCP
import Pipes.Prelude qualified as P

import Request
import Response
import RequestParser
import ServerEnv
import ServerM

server :: MonadIO m => Socket -> Effect (ServerM ServerEnv m) ()
server socket =
  do
    void $ A.parsed requestParser (fromSocket socket bufferSize)
    >-> P.map handleRequest
    >-> P.map encodeResponse
    >-> P.tee P.print
    >-> toSocket socket

handleRequest :: Request -> Response
handleRequest request
  | request.target == "/" = OK
  | otherwise = NotFound
