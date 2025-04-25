module Main
  ( main
  ) where

import Control.Concurrent (forkFinally)
import Control.Exception qualified as E
import Control.Monad (forever)
import Control.Monad.Reader
import Network.Socket hiding (socket)
import Pipes
import Safe (headErr)
import System.IO

import Server
import ServerEnv
import ServerM

main :: IO ()
main = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  let host = "127.0.0.1"
      port = "4221"

  putStrLn $ "Listening on " <> host <> ":" <> port

  let serverEnv = ServerEnv {}

  runTCPServer (Just host) port $
    \clientSocket -> do
      peerName <- getPeerName clientSocket
      putStrLn $ "Accepted connection from " <> show peerName
      flip runReaderT serverEnv . runServerM . runEffect $ server clientSocket

-- See: https://hackage.haskell.org/package/network-3.2.7.0/docs/Network-Socket.html.
runTCPServer
  :: forall a
   . Maybe HostName
  -> ServiceName
  -> (Socket -> IO a)
  -> IO a
runTCPServer mhost port server' = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve :: IO AddrInfo
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
      headErr <$> getAddrInfo (Just hints) mhost (Just port)

    open :: AddrInfo -> IO Socket
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 5
      pure sock

    loop :: Socket -> IO a
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(socket, _addr) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server' socket) (const $ gracefulClose socket 5000)
