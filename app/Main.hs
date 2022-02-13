{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Bot.Telegram.Implement     as TG (parseConfig, withHandle)
import           Bot.Telegram.JSONInstances (TelegramUpdate)
import           Config                     (fetchConfig)
import           Data.Aeson                 (eitherDecodeFileStrict', encode)
import           Data.Function              ((&))
import           Handlers.Bot               (Update (uUpdate), runBot)
import qualified Logger.IO                  as Lio (parseConfig, withHandle)

main = do
  appConfig <- fetchConfig
  Lio.withHandle (Lio.parseConfig appConfig) $ \l ->
    TG.withHandle (TG.parseConfig appConfig) l $ \b ->
      runBot b
  return ()
