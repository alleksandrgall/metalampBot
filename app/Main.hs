{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RankNTypes        #-}
module Main where
import           Bot.Telegram.JSONInstances
import           Config
import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson
import           Data.Aeson.Types           (ToJSON (toJSON))
import           Data.Function              ((&))
import           Data.String                (IsString (fromString))
import           Data.Text
import           GHC.Generics               (Generic)
import           Handlers.Bot               (Command)
import qualified Handlers.Logger            as L
import           Internal.Req               (makeRequest)
import           Internal.Types             (Protocol (Https))
import           Logger.IO

tgRequest :: (MonadCatch m, MonadIO m, ToJSON b, FromJSON a) => String -> L.Handle m -> Maybe b -> Text -> [(Text, Text)] -> m a
tgRequest token hL body method params = do
    makeRequest hL body Https "api.telegram.org" ["bot" <> fromString token, method] params
{-# INLINE tgRequest #-}

initTg :: (MonadCatch m, MonadIO m) => String -> L.Handle m -> m ()
initTg cToken hL = do
    L.info hL $ L.JustText "Getting me"
    me <- tgRequest cToken hL (Nothing :: Maybe String) "getMe" []
    L.info hL $ L.JustText ("Got me: " <> showUser me)
    where
        showUser :: User -> Text
        showUser = pack . show


main = do
  appConfig <- fetchConfig
  withHandle (Config Nothing True Info) $ \hL ->
    initTg (appConfig & appConfigToken) hL
  return ()
