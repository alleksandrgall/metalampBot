{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
module Logger.Logger

where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Text
import           GHC.Generics         (Generic)
import           Prelude              hiding (log)

data LogLevel = Debug | Info | Warning | Error deriving (Show, Generic, Eq, Ord)

type Logger m = LogLevel -> Text -> m ()

class (Monad m) => HasLogger m env | env -> m where
    getLogger :: env -> Logger m

class Monad m => MonadLog m where
    log :: Logger m
    default log :: (MonadTrans t, MonadLog m1, t m1 ~ m) => Logger m
    log lvl msg = lift $ log lvl msg
    logDebug :: Text -> m ()
    logDebug = log Debug
    logInfo :: Text -> m ()
    logInfo = log Info
    logWarning :: Text -> m ()
    logWarning = log Warning
    logError :: Text -> m ()
    logError = log Error

class HasLoggerLevel env where
    getLoggerLevel :: env -> LogLevel

instance HasLoggerLevel LogLevel where
    getLoggerLevel = id

instance (HasLogger m env, HasLoggerLevel env) => MonadLog (ReaderT env m) where
    log lvl msg = do
        envLogger <- asks getLogger
        envLevel <- asks getLoggerLevel
        when (lvl >= envLevel) (lift $ envLogger lvl msg)

