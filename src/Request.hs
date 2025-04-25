module Request
  ( Request (..)
  , HttpMethod (..)
  ) where

import Data.Text (Text)

import HttpHeader

data Request = Request
  { method :: HttpMethod
  , target :: Text
  , version :: Text
  , httpHeaders :: [HttpHeader]
  , body :: Text
  }
  deriving (Show)

data HttpMethod = GET | POST deriving (Eq, Show)
