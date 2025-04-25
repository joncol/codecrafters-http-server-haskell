module HttpHeader
  ( HttpHeader
  ) where

import Data.Text (Text)

type HttpHeader = (Text, Text)
