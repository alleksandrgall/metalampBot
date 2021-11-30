{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config
import Bot.API
import Control.Concurrent.STM ( newTVarIO, modifyTVar', STM, atomically, readTVarIO )
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Logger
import Data.Text as T
import Data.Text.IO as T
import Prelude hiding (log)
import Control.Monad.Writer (Writer, tell, runWriter)



main = do
  logged <- newTVarIO ""
  currentUp <- newTVarIO 1
  let
    myLogger :: LogLevel -> Text -> IO ()
    myLogger lvl text = liftIO $ atomically $ modifyTVar' logged (\s -> s `append` ( pack . show $ lvl) `append` ": " `append` text `append` "\n")
    myEnv = Env {
      logger = myLogger,
      logLevel = Debug,
      currentUpdateId = currentUp
    }
  runReaderT (logDebug "123" >> logInfo "info") myEnv
  readTVarIO logged >>= \t -> mapM_ print (T.lines t)  
  return ()
