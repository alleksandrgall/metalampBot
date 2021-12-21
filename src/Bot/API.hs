{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Bot.API where


import           Control.Concurrent.STM.TVar
import           Control.Monad.Identity      (Identity)
import           Control.Monad.Reader
import           Data.Aeson                  (FromJSON (parseJSON))
import           Data.Aeson.Types            (parseEither)
import           Data.Text                   (Text)
import           Logger

data Env m = Env {
      logger          :: Logger m
    , logLevel        :: LogLevel
    , currentUpdateId :: TVar Int
}

instance Monad m => HasLog m (Env m) where
  getLog = logger

instance HasLogLevel (Env m) where
  getLogLevel = logLevel


type Bot m = ReaderT (Env m) m






