{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Bot.Telegram as TG
import           Config       (fetchConfig)
import           Handlers.Bot (runBot)
import qualified Logger.IO    as Lio


main :: IO ()
main = do
  appConfig <- fetchConfig
  Lio.withHandle (Lio.parseConfig appConfig) $ \l ->
    TG.withHandle (TG.parseConfig appConfig) l $ \b ->
      runBot b
