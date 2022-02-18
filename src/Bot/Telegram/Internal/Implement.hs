module Bot.Telegram.Internal.Implement (Config(..), parseConfig, withHandle) where

import           Bot.Telegram.Internal.Types (TelegramGettable (GSticker),
                                              TelegramMessageSend,
                                              TelegramUpdate, TelegramUser (..))
import           Config
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (void)
import           Control.Monad.Catch         (MonadCatch, MonadMask, bracket)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Aeson                  (FromJSON, KeyValue ((.=)), ToJSON,
                                              Value (Array), encode, object)
import           Data.Aeson.Types            (ToJSON (toJSON))
import qualified Data.ByteString.Lazy        as BS
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import qualified Data.HashMap.Lazy           as HM
import           Data.IORef                  (modifyIORef', newIORef, readIORef,
                                              writeIORef)
import           Data.Int                    (Int64)
import           Data.List
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text, pack)
import           GHC.Exts
import           GHC.Generics                (Generic)
import qualified Handlers.Bot                as B
import qualified Handlers.Logger             as L
import           Internal.Req                (makeRequest, parseResponse)
import           Internal.Types              (Protocol (Https))

parseConfig :: AppConfig -> Config
parseConfig AppConfig {..} = Config {
    cBaseRepeat        = appConfigRepeat & repeatDefaultNumber
  , cStartMes          = appConfigStart & startMessage
  , cHelpMes           = appConfigHelp & helpMessage
  , cRepeatMes         = appConfigRepeat & repeatMessage
  , cRepeatKeyboardMes = appConfigRepeat & repeatKeyboardMes
  , cDelay             = 0
  , cToken             = appConfigToken
}
data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  , cDelay             :: Int
  , cToken             :: String
}

withHandle :: (MonadMask m, MonadIO m) =>
    Config -> L.Handle m -> (B.Handle TelegramGettable TelegramUser m -> m a) -> m a
withHandle Config {..} hL f = do
    offset <- liftIO $ newIORef 0
    userRepeat <- liftIO $ newIORef (mempty :: HM.HashMap TelegramUser Int)
    let c = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes cDelay
        h = B.Handle {
          hConfig             = c
        , hLogger             = hL
        , hInit               = initTg cToken hL
        , hSleep              = return ()
        , hGetUpdates         = getUpdatesTg cToken hL
        , hSendMes            = sendMesTg cToken hL
        , hAnswerCallback     = ansCbTg cToken hL
        , hGetOffset          = getOffsetTg offset
        , hSetOffset          = setOffsetTg offset
        , hInsertUserRepeat   = insertUserRepeatTg userRepeat
        , hGetUserRepeat      = getUserRepeatTg userRepeat
        , hShowUserInfo       = showUiTg
    }
    f h
    where
        showUiTg (TelegramUser (uId, chatId)) = "user_id: " <> (pack . show $ uId) <> ", chat_id: " <> (pack . show $ chatId)
        getOffsetTg ref = liftIO $ readIORef ref
        setOffsetTg ref offset = liftIO $ writeIORef ref offset
        getUserRepeatTg ref ui = (liftIO . readIORef $ ref) <&> HM.lookup ui
        insertUserRepeatTg ref ui r = liftIO $ modifyIORef' ref (HM.insert ui r)

tgRequest :: (MonadCatch m, MonadIO m, ToJSON b) => String -> L.Handle m -> Maybe b -> Text -> [(Text, Text)] -> m BS.ByteString
tgRequest token hL body method params = do
    makeRequest hL body Https "api.telegram.org" ["bot" <> fromString token, method] params
{-# INLINE tgRequest #-}

initTg :: (MonadCatch m, MonadIO m) => String -> L.Handle m -> m ()
initTg token hL = do
    tgRequest token hL (Nothing :: Maybe String) "getMe" []
    tgRequest token hL (Just $ TgCommands
        [("/repeat", "Use to change the repeat number."),
         ("/help"  , "Use to get help."),
         ("/start" , "Use to see a start message")])
         "setMyCommands" []
    return ()

getUpdatesTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Int64 -> m [TelegramUpdate]
getUpdatesTg token hL offset = parseResponse hL =<< tgRequest token hL (Nothing :: Maybe String) "getUpdates"
    [("offset", pack . show $ offset),
     ("timeout", pack . show $ 1)]

sendMesTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> TelegramMessageSend -> m ()
sendMesTg token hL tgMes = case tgMes & B.msContent of
    B.CGettable (GSticker _) -> void $ tgRequest token hL (Just tgMes) "sendSticker" []
    _   -> void $ tgRequest token hL (Just tgMes) "sendMessage" []

ansCbTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Text -> B.CallbackQuery TelegramUser -> m ()
ansCbTg token hL cbMes B.CallbackQuery {..} = void $ tgRequest token hL (Nothing :: Maybe String) "answerCallbackQuery"
    [("callback_query_id", pack cbId),
     ("text", cbMes)]

newtype TgCommands = TgCommands [(Text, Text)] deriving Generic
instance ToJSON TgCommands where
    toJSON (TgCommands ls) = object ["commands" .= Array (fromList (map (\(c, d) -> object ["command" .= c, "description" .= d]) ls))]

