{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Env where




import Logger
import Control.Concurrent.STM.TVar
import Control.Monad.Identity (Identity)

data Env m = Env {
      logger :: Logger m
    , logLevel :: LogLevel
}

instance Monad m => HasLog m (Env m) where
  getLog = logger

instance HasLogLevel (Env m) where
  getLogLevel = logLevel
