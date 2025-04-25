module Options
  ( Options (..)
  , optionsParser
  ) where

import Options.Applicative

newtype Options = Options
  { directory :: FilePath
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "directory"
          <> value "."
          <> help "The directory where files are stored on the server"
      )
