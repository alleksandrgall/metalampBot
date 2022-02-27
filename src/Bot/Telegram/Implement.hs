module Bot.Telegram.Implement (Config(..), parseConfig, withHandle) where

import           Bot.Telegram.Types     (TelegramGettable (GSticker),
                                         TelegramMessageSend,
                                         TelegramResult (..), TelegramUpdate,
                                         TelegramUpdateWithId (..),
                                         TelegramUserInfo)
import           Config
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (void)
import           Control.Monad.Catch    (MonadCatch, MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson             (FromJSON, KeyValue ((.=)), ToJSON,
                                         Value (Array), encode, object)
import           Data.Aeson.Types       (ToJSON (toJSON))
import qualified Data.ByteString.Lazy   as BS
import           Data.Function          ((&))
import           Data.Functor           ((<&>))
import qualified Data.HashMap.Lazy      as HM
import           Data.IORef             (modifyIORef', newIORef, readIORef,
                                         writeIORef)
import           Data.Int               (Int64)
import           Data.List
import           Data.String            (IsString (fromString))
import           Data.Text              (Text, pack)
import           GHC.Exts
import           GHC.Generics           (Generic)
import qualified Handlers.Bot           as B
import qualified Handlers.Logger        as L
import           Internal.Req           (makeRequest, parseResponse)
import           Internal.Types         (Protocol (Https))
import           Internal.Utils

apiVersion = 5.131

parseConfig :: AppConfig -> Config
parseConfig AppConfig {..} = Config {
    cBaseRepeat        = appConfigRepeat & repeatDefaultNumber
  , cStartMes          = appConfigStart & startMessage
  , cHelpMes           = appConfigHelp & helpMessage
  , cRepeatMes         = appConfigRepeat & repeatMessage
  , cRepeatKeyboardMes = appConfigRepeat & repeatKeyboardMes
  , cToken             = appConfigToken
}
data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  , cToken             :: String
}

withHandle :: (MonadMask m, MonadIO m) =>
    Config -> L.Handle m -> (B.Handle TelegramGettable TelegramUserInfo m -> m a) -> m a
withHandle Config {..} hL f = do
    offset <- liftIO $ newIORef 0
    userRepeat <- liftIO $ newIORef (mempty :: HM.HashMap TelegramUserInfo Int)
    let c = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes
        h = B.Handle {
          hConfig             = c
        , hLogger             = hL
        , hInit               = initTg cToken hL
        , hGetUpdates         = getUpdatesTg cToken hL
        , hSendMes            = sendMesTg cToken hL
        , hAnswerCallback     = ansCbTg cToken hL
        , hGetOffset          = getOffsetTg offset
        , hSetOffset          = setOffsetTg offset
        , hInsertUserRepeat   = insertUserRepeatTg userRepeat
        , hGetUserRepeat      = getUserRepeatTg userRepeat
    }
    f h
    where
        getOffsetTg ref = liftIO $ readIORef ref
        setOffsetTg ref offset = liftIO $ writeIORef ref offset
        getUserRepeatTg ref ui = (liftIO . readIORef $ ref) <&> HM.lookup ui
        insertUserRepeatTg ref ui r = liftIO $ modifyIORef' ref (HM.insert ui r)

tgRequest :: (MonadCatch m, MonadIO m, ToJSON b) => String -> L.Handle m -> Maybe b -> Text -> [(Text, Text)] -> m BS.ByteString
tgRequest token hL body method params = do
    makeRequest hL body Https "api.telegram.org" ["bot" <> fromString token, method] params
{-# INLINE tgRequest #-}

newtype TgCommands = TgCommands [(Text, Text)] deriving Generic
instance ToJSON TgCommands where
    toJSON (TgCommands ls) = object ["commands" .= Array (fromList (map (\(c, d) -> object ["command" .= c, "description" .= d]) ls))]

initTg :: (MonadCatch m, MonadIO m) => String -> L.Handle m -> m ()
initTg token hL = do
    tgRequest token hL (Nothing :: Maybe String) "getMe" []
    tgRequest token hL (Just $ TgCommands
        [("/repeat", "Use to change the repeat number."),
         ("/help"  , "Use to get help."),
         ("/start" , "Use to see a start message")])
         "setMyCommands" []
    return ()

getUpdatesTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Int64 -> m (Maybe Int64, [TelegramUpdate])
getUpdatesTg token hL offset = do
    TelegramResult idUpds <- parseResponse hL =<< tgRequest token hL (Nothing :: Maybe String) "getUpdates"
     [("offset", pack . show $ offset),
     ("timeout", pack . show $ 2)]
    let (lastUId, upds) = foldr (\TelegramUpdateWithId{..} (curMax, res) ->
            if curMax < Just uId then (Just uId, uCont:res) else (curMax, uCont:res)) (Nothing, []) (idUpds :: [TelegramUpdateWithId])
    return ((+1) <$> lastUId, upds)

sendMesTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> TelegramMessageSend -> m ()
sendMesTg token hL tgMes = case tgMes & B.msContent of
    B.CGettable (GSticker _) -> void $ tgRequest token hL (Just tgMes) "sendSticker" []
    _   -> void $ tgRequest token hL (Just tgMes) "sendMessage" []

ansCbTg :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Text -> B.CallbackQuery TelegramUserInfo  -> m ()
ansCbTg token hL cbMes B.CallbackQuery {..} = void $ tgRequest token hL (Nothing :: Maybe String) "answerCallbackQuery"
    [("callback_query_id", pack cbId),
     ("text", cbMes)]

