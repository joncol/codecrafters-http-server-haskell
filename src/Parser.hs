module Parser
  ( parseRequest
  , parseHeader
  ) where

import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Monoid (Any (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import HttpHeader
import Request

parseRequest :: Parser Request
parseRequest = do
  void "GET"
  void A.space
  target <- TE.decodeLatin1 <$> takeTill A.isSpace_w8
  void A.space
  version <- TE.decodeLatin1 <$> takeTill A.isEndOfLine
  void A.endOfLine
  httpHeaders <- parseHeader `sepBy` A.endOfLine
  void A.endOfLine
  pure
    Request
      { method = GET
      , target
      , version
      , httpHeaders
      , body = ""
      }

parseHeader :: Parser HttpHeader
parseHeader = do
  name <-
    T.pack
      <$> many1
        ( A.satisfy $
            anyPass
              [ A.isAlpha_iso8859_15
              , A.isDigit
              , A.inClass "-_"
              ]
        )
  void ":"
  void $ many' A.space
  value <-
    T.pack
      <$> many1
        ( A.satisfy $
            anyPass
              [ A.isAlpha_iso8859_15
              , A.isDigit
              , A.inClass "-_ :;.,\\/\"'?!(){}[]@<>=+*#$&`|~^%"
              ]
        )
  pure (name, value)

anyPass :: [a -> Bool] -> a -> Bool
anyPass ps = getAny . foldMap (fmap Any) ps
