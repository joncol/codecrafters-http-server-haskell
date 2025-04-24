module Main
  ( main
  ) where

import Control.Monad (forever)
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as BC
import Network.Socket qualified as Sock
import Pipes
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

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port.
  addrInfo <- Sock.getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- Sock.socket (Sock.addrFamily $ head addrInfo) Sock.Stream Sock.defaultProtocol
  Sock.bind serverSocket $ Sock.addrAddress $ head addrInfo
  Sock.listen serverSocket 5

  let serverEnv = ServerEnv {}

  -- Accept connections and handle them forever.
  forever $ do
    (socket, addr) <- Sock.accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show addr) <> "."
    flip runReaderT serverEnv . runServerM . runEffect $ server socket
