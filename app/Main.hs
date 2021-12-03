{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Env
import Control.Concurrent.STM ( newTVarIO, modifyTVar', STM, atomically, readTVarIO )
import Control.Monad.Reader
import Logger
import Data.Text as T
import Data.Text.Lazy.Encoding as E
import Prelude hiding (log)
import Bot.ResponseTypes
import Data.Aeson.Types
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Except (runExcept, runExceptT)
import Data.String




main = do
  config <- fetchConfig
  let
    decodeMessage :: BS.ByteString -> Either String Message
    decodeMessage = eitherDecode
  messageText <- (decodeMessage <$> BS.readFile "temp/messageText.json")
  putStrLn $ show messageText

  -- logged <- newTVarIO ""
  -- currentUp <- newTVarIO 1
  -- let
  --   myLogger :: LogLevel -> Text -> IO ()
  --   myLogger lvl text = atomically $ modifyTVar' logged (\s -> s `append` ( pack . show $ lvl) `append` ": " `append` text `append` "\n")
  --   myEnv = Env {
  --     logger = myLogger,
  --     logLevel = Debug
  --   }
  -- runReaderT (logDebug "123" >> logInfo "info") myEnv
  -- readTVarIO logged >>= \t -> mapM_ print (T.lines t)  
  return ()
