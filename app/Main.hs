{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Bot.ResponseTypes
import Control.Concurrent.STM ( newTVarIO, modifyTVar', STM, atomically, readTVarIO )
import Control.Monad.Reader
import Logger
import Data.Text as T
import Data.Text.Lazy.Encoding as E
import Prelude hiding (log)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Except (runExcept, runExceptT)


type Token = String

-- getMe :: Token ->  IO User
-- getMe = do


main = do
  config <- fetchConfig
  logOut <- newTVarIO ""
  let
    decodeMessage :: BS.ByteString -> Either String Message
    decodeMessage = eitherDecode
    decodeUpdate :: BS.ByteString -> Either String Update
    decodeUpdate = eitherDecode
    decodeResponseUpdates :: BS.ByteString -> Either String (Response [Update])
    decodeResponseUpdates = decodeResponseUpdates
  
  print "----------------------------------------------------"
  print "messageText"
  messageText <- decodeMessage <$> BS.readFile "temp/messageText.json"
  print messageText
  print "----------------------------------------------------"
  print "messagePhoto"
  messagePhoto <- decodeMessage <$> BS.readFile "temp/messagePhoto.json"
  print messagePhoto
  print "----------------------------------------------------"  
  print "updatePhoto"
  updatePhoto <- decodeUpdate <$> BS.readFile "temp/updatePhoto.json"
  print updatePhoto
  print "----------------------------------------------------"
  print "updateMessage"
  updateMessage <- decodeUpdate <$> BS.readFile "temp/updateMessage.json"
  print updateMessage
  print "----------------------------------------------------"
  print "responseUpdatesNoQuery"
  responseUpdatesNoQuery <- decodeResponseUpdates <$> BS.readFile "temp/responseUpdatesNoQuery.json"
  print responseUpdatesNoQuery
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
