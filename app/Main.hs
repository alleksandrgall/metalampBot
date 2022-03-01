{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Bot.Telegram         as TG
import qualified Bot.VK               as VK
import           Bot.VK.Types
import           Config               (fetchConfig)
import           Data.Aeson           (decodeFileStrict', encode)
import qualified Data.ByteString.Lazy as B
import           Handlers.Bot         (Keyboard (Keyboard), runBot)
import qualified Logger.IO            as Lio

kb = Keyboard ["1", "2", "3", "4", "5"]

main :: IO ()
main = do

  writeFile "temp/VKKeyboardEncode.json" (show . encode $ Button "1")
  -- appConfig <- fetchConfig
  -- Lio.withHandle (Lio.parseConfig appConfig) $ \l ->
  --   VK.withHandle (VK.parseConfig appConfig) l $ \b ->
  --     runBot b

