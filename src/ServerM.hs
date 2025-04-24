module ServerM
  ( ServerM (..)
  , bufferSize
  )
where

import Control.Monad.Reader

newtype ServerM r m a = ServerM
  { runServerM :: ReaderT r m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadIO
    )

bufferSize :: Integral a => a
bufferSize = 4096
