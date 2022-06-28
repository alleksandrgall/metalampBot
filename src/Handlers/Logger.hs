module Handlers.Logger
  ( LogLevel (..),
    Handle (..),
    Config (..),
    log,
    debug,
    info,
    warning,
    error,
  )
where

import Control.Monad (when)
import Data.Function ((&))
import Data.Text (Text)
import Prelude hiding (error, log)

data LogLevel = Debug | Info | Warning | Error deriving (Show, Eq, Ord)

data Handle m = Handle
  { hConfig :: Config,
    hLogMessage :: LogLevel -> Text -> m ()
  }

newtype Config = Config
  { cDefaultLogLevel :: LogLevel
  }

log :: (Monad m) => Handle m -> LogLevel -> Text -> m ()
log Handle {..} lvl m = when (lvl >= (hConfig & cDefaultLogLevel)) (hLogMessage lvl m)

debug, info, warning, error :: (Monad m) => Handle m -> Text -> m ()
debug h = log h Debug
info h = log h Info
warning h = log h Warning
error h = log h Error
