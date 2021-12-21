{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module Logger.Logger

where

import Control.Monad.Writer
import Data.Text
import GHC.Generics ( Generic )
import Control.Monad.Reader
import Prelude hiding (log)

data LogLevel = Debug | Info | Warning | Error deriving (Show, Generic, Eq, Ord)

type Logger m = LogLevel -> Text -> m ()

class (Monad m) => HasLog m env | env -> m where
    getLog :: env -> Logger m

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

class HasLogLevel env where
    getLogLevel :: env -> LogLevel

instance HasLogLevel LogLevel where
    getLogLevel = id

instance (HasLog m env, HasLogLevel env) => MonadLog (ReaderT env m) where
    log lvl msg = do
        envLogger <- asks getLog
        envLevel <- asks getLogLevel
        when (lvl >= envLevel) (lift $ envLogger lvl msg) 

