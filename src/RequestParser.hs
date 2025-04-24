module RequestParser
  ( requestParser
  ) where

import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (isSpace_w8, space)
import Data.Text.Encoding qualified as TE

import Request

requestParser :: Parser Request
requestParser = do
  void $ string "GET"
  void space
  target <- TE.decodeLatin1 <$> takeTill isSpace_w8
  pure
    Request
      { method = GET
      , target
      , httpVersion = "HTTP/1.1"
      , httpHeaders = []
      , body = ""
      }
