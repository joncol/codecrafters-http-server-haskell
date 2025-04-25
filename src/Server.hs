module Server
  ( server
  ) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Pipes
import Pipes.Attoparsec qualified as A
import Pipes.Network.TCP
import Pipes.Prelude qualified as P
import TextShow

import Request
import RequestParser
import Response
import ServerEnv
import ServerM

server :: MonadIO m => Socket -> Effect (ServerM ServerEnv m) ()
server socket =
  do
    void $ A.parsed requestParser (fromSocket socket bufferSize)
    >-> P.map handleRequest
    >-> P.map encodeResponse
    >-> toSocket socket

handleRequest :: Request -> Response
handleRequest request
  | request.target == "/" = emptyResponse OK
  | "/echo/" `T.isPrefixOf` request.target =
      let str = fromMaybe "" $ T.stripPrefix "/echo/" request.target
      in  (emptyResponse OK)
            { headers =
                [ ("Content-Type", "text/plain")
                , ("Content-Length", showt (T.length str))
                ]
            , body = str
            }
  | otherwise = emptyResponse NotFound
  where
    emptyResponse :: HttpStatus -> Response
    emptyResponse status = Response {status, headers = [], body = ""}
