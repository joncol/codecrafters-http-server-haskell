module Response
  ( Response (..)
  , encodeResponse
  , HttpStatus (..)
  , encodeHttpStatus
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Encoding
import Data.Text (Text)

import HttpHeader

data Response = Response
  { status :: HttpStatus
  , headers :: [HttpHeader]
  , body :: Text
  }
  deriving (Show)

data HttpStatus
  = -- | 200 OK
    OK
  | -- | 201 Created
    Created
  | -- | 404 Not Found
    NotFound
  deriving (Show)

encodeResponse :: Response -> BS.ByteString
encodeResponse Response {..} =
  encodeHttpStatus status
    <> foldMap encodeHeader headers
    <> "\r\n"
    <> encode latin1 body

-- TODO: Don't repeat version here.
encodeHttpStatus :: HttpStatus -> BS.ByteString
encodeHttpStatus OK = "HTTP/1.1 200 OK\r\n"
encodeHttpStatus Created = "HTTP/1.1 201 Created\r\n"
encodeHttpStatus NotFound = "HTTP/1.1 404 Not Found\r\n"

encodeHeader :: HttpHeader -> BS.ByteString
encodeHeader (name, value) = encode latin1 name <> ": " <> encode latin1 value <> "\r\n"
