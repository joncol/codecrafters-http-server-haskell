module Request
  ( Request (..)
  , HttpMethod (..)
  ) where

import Data.Text (Text)

data Request = Request
  { method :: HttpMethod
  , target :: Text
  , httpVersion :: Text
  , httpHeaders :: [HttpHeader]
  , body :: Text
  }
  deriving (Show)

data HttpMethod = GET deriving (Eq, Show)

type HttpHeader = (Text, Text)
