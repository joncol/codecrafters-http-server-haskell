module ServerEnv
  ( ServerEnv (..)
  ) where

import Options

newtype ServerEnv = ServerEnv
  { options :: Options
  }
  deriving (Show)
