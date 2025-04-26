module HttpHeader
  ( HttpHeader
  , getHeaderValue
  ) where

import Data.Foldable qualified as Foldable
import Data.Text (Text)
import Data.Text qualified as T

type HttpHeader = (Text, Text)

getHeaderValue :: Text -> [HttpHeader] -> Maybe Text
getHeaderValue name headers =
  snd <$> Foldable.find ((== name) . T.toLower . fst) headers
