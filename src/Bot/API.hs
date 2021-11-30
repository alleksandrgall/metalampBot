{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bot.API where

import Network.HTTP.Req
import Bot.ResponseTypes
import Control.Monad.Reader 
import Config
import Data.Text ( Text )
import Data.Aeson (FromJSON (parseJSON))
import Control.Exception (throwIO)
import Control.Monad.RWS
import Data.Aeson.Types (parseEither)
import Control.Monad.Except
import Control.Monad.State
import Logger
import Control.Concurrent.STM.TVar
import Control.Monad.Identity (Identity)

data Env m = Env {
      logger :: Logger m
    , logLevel :: LogLevel
    , currentUpdateId :: TVar Int
} 

instance Monad m => HasLog m (Env m) where
  getLog = logger

instance HasLogLevel (Env m) where
  getLogLevel = logLevel


type Bot m = ReaderT (Env m) m



