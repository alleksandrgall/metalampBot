module Handlers.Logger
  ( LogLevel(..)
  , Handle(..)
  , Config(..)
  , log
  , debug
  , info
  , warning
  , error) where

import           Data.Function ((&))
import           Data.Text     (Text, pack)
import           Prelude       hiding (error, log)

data LogLevel = Debug | Info | Warning | Error deriving (Show, Eq, Ord)

data Handle m = Handle
    {
      hConfig     :: Config
    , hLogMessage :: LogLevel -> Text -> m ()
    }

newtype Config = Config
    {
      cDefaultLogLevel :: LogLevel
    }

log :: (Monad m) => Handle m -> LogLevel -> String -> m ()
log Handle {..} lvl m
  | lvl >= (hConfig & cDefaultLogLevel) = hLogMessage lvl (pack m)
  | otherwise = return ()

debug, info, warning, error :: (Monad m) => Handle m -> String -> m ()
debug h = log h Debug
info h = log h Info
warning h = log h Warning
error h = log h Error
