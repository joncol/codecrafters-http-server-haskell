module Response
  ( Response (..)
  , encodeResponse
  ) where

import Data.ByteString qualified as BS

data Response
  = -- | 200 OK
    OK
  | -- | 404 Not Found
    NotFound
  deriving (Show)

encodeResponse :: Response -> BS.ByteString
encodeResponse OK = "HTTP/1.1 200 OK\r\n\r\n"
encodeResponse NotFound = "HTTP/1.1 404 Not Found\r\n\r\n"
