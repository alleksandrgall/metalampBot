module Handlers.Logger
  ( LogLevel(..)
  , Handle(..)
  , Config(..)
  , MessageType(..)
  , log
  , debug
  , info
  , warning
  , error) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Function              ((&))
import           Data.Text                  (Text)
import           Prelude                    hiding (error, log)


data LogLevel = Debug | Info | Warning | Error deriving (Show, Eq, Ord)

data MessageType = WithBs Text ByteString | JustText Text

data Handle m = Handle
    {
      hConfig     :: Config
    , hLogMessage :: LogLevel -> MessageType -> m ()
    }

newtype Config = Config
    {
      cDefaultLogLevel :: LogLevel
    }

log :: (Monad m) => Handle m -> LogLevel -> MessageType -> m ()
log Handle {..} lvl m
  | lvl >= (hConfig & cDefaultLogLevel) = hLogMessage lvl m
  | otherwise = return ()

debug, info, warning, error :: (Monad m) => Handle m -> MessageType -> m ()
debug h = log h Debug
info h = log h Info
warning h = log h Warning
error h = log h Error



