{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Bot.Telegram         as TG
import qualified Bot.VK               as VK
import           Bot.VK.Types
import           Config               (AppConfig (appConfigGroupId, appConfigMessenger, appConfigToken),
                                       GroupId (GroupId), Messenger (Tele, VK),
                                       fetchConfig)
import           Control.Monad        (when)
import           Data.Aeson           (eitherDecode, eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as B
import           Data.Function        ((&))
import           Handlers.Bot         (CallbackQuery, Keyboard (Keyboard),
                                       runBot)
import qualified Handlers.Logger      as L
import qualified Logger.IO            as Lio


main :: IO ()
main = do
  -- print =<< (eitherDecodeFileStrict "temp/VKCallback.json" :: IO (Either String (CallbackQuery VKUserInfo)))
  appConfig <- fetchConfig
  Lio.withHandle (Lio.parseConfig appConfig) $ \l -> do
    when ((appConfig & appConfigToken) == "") $ L.warning l $ L.JustText "Access token must be specified for correct behavior"
    case appConfig & appConfigMessenger of
      VK -> do
        when ((appConfig & appConfigGroupId) == GroupId (-1)) $
          L.warning l $ L.JustText "Group Id must be specified for VK"
        L.info l $ L.JustText "Launching VK bot..."
        VK.withHandle (VK.parseConfig appConfig) l $ \bot ->
          runBot bot
      Tele -> do
        L.info l $ L.JustText "Launching Telegram bot..."
        TG.withHandle (TG.parseConfig appConfig) l $ \bot ->
            runBot bot

