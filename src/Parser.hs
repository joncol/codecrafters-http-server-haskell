module Parser
  ( parseRequest
  , parseHeader
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Trans.Maybe
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Read qualified as T

import HttpHeader
import Request

parseRequest :: Parser Request
parseRequest = do
  method <- "GET" $> GET <|> "POST" $> POST
  void A.space
  target <- TE.decodeLatin1 <$> takeTill A.isSpace_w8
  void A.space
  version <- TE.decodeLatin1 <$> takeTill A.isEndOfLine
  void A.endOfLine
  httpHeaders <- parseHeader `sepBy` A.endOfLine
  void A.endOfLine
  void A.endOfLine
  mContentLength :: Maybe Int <-
    runMaybeT $ do
      headerValue <-
        hoistMaybe $
          T.unpack . snd
            <$> Foldable.find
              (\(name, _) -> T.toLower name == "content-length")
              httpHeaders
      hoistMaybe $ T.readMaybe headerValue

  let contentLength = fromMaybe 0 mContentLength
  body <- T.pack <$> count contentLength A.anyChar
  pure
    Request
      { method
      , target
      , version
      , httpHeaders
      , body
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
