module Bot.Telegram.Implement where

import           Bot.Telegram.JSONInstances
import           Control.Concurrent         (threadDelay)
import           Control.Monad.Catch        (MonadCatch, MonadMask, bracket)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Function              ((&))
import qualified Data.HashMap.Lazy          as HM
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Data.Int                   (Int64)
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

withHandle :: (MonadMask m, MonadIO m) =>
    Config -> L.Handle m -> (B.Handle MessageText MessageSticker m -> m a) -> m a
withHandle Config {..} hL f = bracket (do
    offset <- liftIO $ newIORef 0
    userRepeat <- liftIO $ newIORef (mempty :: HM.HashMap B.UserInfo Int)
    return B.Handle {
          hConfig = undefined
        , hLogger = hL
        , hInit   = initTg cToken hL
        , hSleep  = liftIO $ threadDelay cDelay
        , hGetUpdates = getUpdatesTg cToken hL
        , hSendMes            = sendMesTg cToken hL
        , hSendKeyboard       = sendKeyboardTg cToken hL
        , hAnsCB              = undefined
        , hGetOffset          = undefined
        , hSetOffset          = undefined
        , hInsertUserRepeat   = undefined
        , hGetUserRepeat      = undefined
    }
    )
    undefined
    undefined
    where
        getOffsetTg ref = liftIO $ readIORef ref
        setOffsetTg ref offset = liftIO $ writeIORef offset ref
        getUserRepeatTg ref = liftIO $ readIORef ref


initTg :: (MonadCatch m, MonadIO m) => String -> L.Handle m -> m ()
initTg token hL = do
    L.info hL $ L.JustText "Getting me"
    me <- tgRequest token hL (Nothing :: Maybe String) "getMe" []
    L.info hL $ L.JustText ("Got me: " <> (pack . show $ (me :: User)))

getUpdatesTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Int64 -> m [TelegramUpdate]
getUpdatesTg token hL offset = tgRequest token hL (Nothing :: Maybe String) "getUpdates" [("offset", pack . show $ offset)]

sendMesTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> TelegramMessage -> m ()
sendMesTg token hL tgMes = case tgMes & B.mMessageContent of
    B.MCSticker _ -> tgRequest token hL (Just tgMes) "sendSticker" []
    B.MCText _    -> tgRequest token hL (Just tgMes) "sendMessage" []

sendKeyboardTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> B.KeyboardMessage -> m ()
sendKeyboardTg token hL tgKbMes = tgRequest token hL (Just tgKbMes) "sendMessage" []

-- | TODO Привлекает внимание сколько берет на себя эта часть логики, возможно имеет смысл
-- чутка ее грануляризировать или даже переделать полностью логику клавиатуры
ansCbTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Text -> B.Callbackquery -> m ()
ansCbTg token hL cbMes B.Callbackquery {..} = undefined

