module Bot.Telegram.Implement where

import           Bot.Telegram.JSONInstances
import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text, pack)
import qualified Handlers.Bot               as B
import qualified Handlers.Logger            as L
import           Internal.Req               (makeRequest)
import           Internal.Types             (Protocol (Https))

data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  -- | Delay in microsec
  , cDelay             :: Int
  , cToken             :: String
}
tgRequest :: (MonadCatch m, MonadIO m, ToJSON b, FromJSON a) => String -> L.Handle m -> Maybe b -> Text -> [(Text, Text)] -> m a
tgRequest token hL body method params = do
    makeRequest hL body Https "api.telegram.org" ["bot" <> fromString token, method] params
{-# INLINE tgRequest #-}

withHandle :: (MonadCatch m, MonadIO m) =>
    Config -> L.Handle m -> (B.Handle m MessageText MessageSticker -> m a) -> m a
withHandle Config {..} hLogger = do
    undefined

initTg :: (MonadCatch m, MonadIO m) => String -> L.Handle m -> m ()
initTg cToken hL = do
    L.info hL $ L.JustText "Getting me"
    me <- tgRequest cToken hL (Nothing :: Maybe String) "getMe" []
    L.info hL $ L.JustText ("Got me: " <> showUser me)
    where
        showUser :: User -> Text
        showUser = pack . show
