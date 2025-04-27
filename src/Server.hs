module Server
  ( server
  ) where

import Codec.Compression.GZip qualified as GZip
import Control.Monad (when)
import Control.Monad.Reader
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Encoding qualified as BSE
import Data.ByteString.Lazy.Encoding qualified as BSLE
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
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
    >-> P.mapM (\request -> (request,) <$> handleRequest request)
    >-> P.map (uncurry handleConnectionCloseHeader)
    >-> takeWhile'' (not . containsCloseHeader . Response.headers)
    >-> P.map encodeResponse
    >-> toSocket socket

-- | Passes elements downstream until a predicate returns false for an element.
--
-- Note that this function also passes through the first element that caused the
-- predicate to return false, but no further elements.
takeWhile'' :: Functor m => (a -> Bool) -> Pipe a a m ()
takeWhile'' predicate = go
  where
    go = do
      a <- await
      yield a
      when (predicate a) go

handleRequest :: MonadIO m => Request -> ServerM ServerEnv m Response
handleRequest request
  | "/" == request.target =
      pure
        (emptyResponse OK)
          { Response.headers = [("Content-Length", "0")]
          }
  | "/echo/" `T.isPrefixOf` request.target =
      let mAcceptEncodings =
            parseOnly parseHeaderValueStringList . BSE.encode BSE.latin1
              <$> getHeaderValue "Accept-Encoding" request.headers
          mAcceptEncodings' =
            fromRight (error "couldn't parse string list")
              <$> mAcceptEncodings
          doGZipCompress =
            ( any (\enc -> T.toLower enc == "gzip")
                <$> mAcceptEncodings'
            )
              == Just True
          mContentEncoding =
            if doGZipCompress
              then Just ("Content-Encoding", "gzip")
              else Nothing
          message = fromMaybe "" $ T.stripPrefix "/echo/" request.target
          compressor =
            if doGZipCompress
              then
                TL.toStrict
                  . BSLE.decode BSLE.latin1
                  . GZip.compress
                  . BSLE.encode BSLE.latin1
                  . TL.fromStrict
              else id
          message' = compressor message
      in  pure
            (emptyResponse OK)
              { Response.headers =
                  [ ("Content-Type", "text/plain")
                  , ("Content-Length", showt $ T.length message')
                  ]
                    <> catMaybes [mContentEncoding]
              , body = message'
              }
  | "/user-agent" == request.target =
      let body = fromMaybe "" $ getHeaderValue "User-Agent" request.headers
      in  pure
            (emptyResponse OK)
              { Response.headers =
                  [ ("Content-Type", "text/plain")
                  , ("Content-Length", showt $ T.length body)
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

handleConnectionCloseHeader :: Request -> Response -> Response
handleConnectionCloseHeader request response =
  if containsCloseHeader request.headers
    then
      response
        { Response.headers =
            response.headers
              <> [("Connection", "close")]
        }
    else response

containsCloseHeader :: [HttpHeader] -> Bool
containsCloseHeader headers =
  let connection = fromMaybe "" $ getHeaderValue "Connection" headers
  in  T.toLower connection == "close"
