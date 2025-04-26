module Server
  ( server
  ) where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Pipes
import Pipes.Attoparsec qualified as A
import Pipes.Network.TCP
import Pipes.Prelude qualified as P
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import TextShow

import HttpHeader
import Options
import Parser
import Request
import Response
import ServerEnv
import ServerM

server :: MonadIO m => Socket -> Effect (ServerM ServerEnv m) ()
server socket =
  do
    void $ A.parsed parseRequest (fromSocket socket bufferSize)
    >-> P.mapM handleRequest
    >-> P.map encodeResponse
    >-> P.tee P.print
    >-> toSocket socket

handleRequest :: MonadIO m => Request -> ServerM ServerEnv m Response
handleRequest request
  | "/" == request.target = pure $ emptyResponse OK
  | "/echo/" `T.isPrefixOf` request.target =
      let body = fromMaybe "" $ T.stripPrefix "/echo/" request.target
      in  pure
            (emptyResponse OK)
              { Response.headers =
                  [ ("Content-Type", "text/plain")
                  , ("Content-Length", showt (T.length body))
                  ]
              , body
              }
  | "/user-agent" == request.target =
      let body = fromMaybe "" $ getHeaderValue "user-agent" request.headers
      in  pure
            (emptyResponse OK)
              { Response.headers =
                  [ ("Content-Type", "text/plain")
                  , ("Content-Length", showt (T.length body))
                  ]
              , body
              }
  | "/files/" `T.isPrefixOf` request.target && request.method == GET = do
      let mFilename = T.stripPrefix "/files/" request.target
      case mFilename of
        Just filename -> do
          env <- ask
          let fullPath = env.options.directory </> T.unpack filename
          fileExists <- liftIO $ doesFileExist fullPath
          if fileExists
            then do
              contents <- liftIO $ TIO.readFile fullPath
              pure $
                (emptyResponse OK)
                  { Response.headers =
                      [ ("Content-Type", "application/octet-stream")
                      , ("Content-Length", showt $ T.length contents)
                      ]
                  , body = contents
                  }
            else pure $ emptyResponse NotFound
        Nothing -> pure $ emptyResponse NotFound
  | "/files/" `T.isPrefixOf` request.target && request.method == POST = do
      let mFilename = T.stripPrefix "/files/" request.target
      case mFilename of
        Just filename -> do
          env <- ask
          let fullPath = env.options.directory </> T.unpack filename
          liftIO $ TIO.writeFile fullPath request.body
          pure $
            (emptyResponse Created)
              { Response.headers = [("Content-Length", "0")]
              }
        Nothing -> pure $ emptyResponse NotFound
  | otherwise = pure $ emptyResponse NotFound
  where
    emptyResponse :: HttpStatus -> Response
    emptyResponse status = Response {status, headers = [], body = ""}
