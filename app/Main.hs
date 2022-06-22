module Main where

import qualified Bot.Telegram as TG
import qualified Bot.VK as VK
import Config
import Control.Monad (when)
import Data.Function ((&))
import Handlers.Bot (runBot)
import qualified Handlers.Logger as L
import qualified Logger.IO as Lio

main :: IO ()
main = do
  appConfig <- fetchConfig
  Lio.withHandle (Lio.parseConfig appConfig) $ \l -> do
    when ((appConfig & appConfigToken) == "") $ L.warning l "Access token must be specified for correct behavior"
    case appConfig & appConfigMessenger of
      VK -> do
        when ((appConfig & appConfigGroupId) == GroupId (-1)) $
          L.warning l "Group Id must be specified for VK"
        L.info l "Launching VK bot..."
        VK.withHandle (VK.parseConfig appConfig) l $ \bot ->
          runBot bot
      Tele -> do
        L.info l "Launching Telegram bot..."
        TG.withHandle (TG.parseConfig appConfig) l $ \bot ->
          runBot bot
