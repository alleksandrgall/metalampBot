{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Env.Env where

import           Logger

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Reader   (ReaderT)
import           Data.Text              (Text)

type RT m = ReaderT (Env m) m

data Env m = Env
    {
          logMessage   :: Logger (RT m)
        
    }

instance (Monad m) => HasLogger (RT m) (Env m) where
    getLogger = logMessage

instance HasLoggerLevel (Env m) where
    getLoggerLevel = undefined 