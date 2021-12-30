module Handlers.Logger
 (LogLevel(..)
 ,Handle(..)
 ,Config(..)
 ,)
 where

import Data.Text (Text)
import Data.Function ((&))
import Prelude hiding (log)


data LogLevel = Debug | Info | Warning | Error deriving (Show, Eq, Ord)

data Handle m = Handle 
    {
      hConfig :: Config
    , hLogMessage :: LogLevel -> Text -> m ()  
    }

data Config = Config 
    {
      cDefaultLogLevel :: LogLevel
    }

log :: (Monad m) => Handle m -> LogLevel -> Text -> m ()
log Handle {..} lvl m 
  | lvl >= (hConfig & cDefaultLogLevel) = hLogMessage lvl m
  | otherwise = return ()

debug, info, warning, error :: (Monad m) => Handle m -> Text -> m ()
debug h = log h Debug 
info h = log h Info 
warning h = log h Warning 
error h = log h Error



