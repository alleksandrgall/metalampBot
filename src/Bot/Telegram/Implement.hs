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
import           Data.String            (IsString (fromString))
import           Data.Text              (Text, pack)
import           GHC.Exts
import           GHC.Generics           (Generic)
import qualified Handlers.Bot           as B
import qualified Handlers.Logger        as L
import           Internal.Req           (makeRequest, parseResponse)
import           Prelude                hiding (init)

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
        , hInit               = init cToken hL
        , hGetUpdates         = getUpdates cToken hL
        , hSendMes            = sendMes cToken hL
        , hAnswerCallback     = ansCb cToken hL
        , hGetOffset          = getOffset offset
        , hSetOffset          = setOffset offset
        , hInsertUserRepeat   = insertUserRepeat userRepeat
        , hGetUserRepeat      = getUserRepeat userRepeat
    }
    f h
    where
        getOffset ref = liftIO $ readIORef ref
        setOffset ref offset = liftIO $ writeIORef ref offset
        getUserRepeat ref ui = (liftIO . readIORef $ ref) <&> HM.lookup ui
        insertUserRepeat ref ui r = liftIO $ modifyIORef' ref (HM.insert ui r)

request :: (MonadCatch m, MonadIO m, ToJSON b) => String -> L.Handle m -> Maybe b -> Text -> [(Text, Text)] -> m BS.ByteString
request token hL body method = makeRequest hL body "api.telegram.org" ["bot" <> fromString token, method]
{-# INLINE request #-}

newtype Commands = Commands [(Text, Text)] deriving Generic
instance ToJSON Commands where
    toJSON (Commands ls) = object ["commands" .= Array (fromList (map (\(c, d) -> object ["command" .= c, "description" .= d]) ls))]

init :: (MonadCatch m, MonadIO m) => String -> L.Handle m -> m ()
init token hL = do
    request token hL (Just $ Commands
        [("/repeat", "Use to change the repeat number."),
         ("/help"  , "Use to get help."),
         ("/start" , "Use to see a start message")])
         "setMyCommands" []
    return ()

getUpdates :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Int64 -> m (Maybe Int64, [TelegramUpdate])
getUpdates token hL offset = do
    TelegramResult idUpds <- parseResponse hL =<< request token hL (Nothing :: Maybe String) "getUpdates"
     [("offset", pack . show $ offset),
     ("timeout", "2")]
    let (lastUId, upds) = foldr (\TelegramUpdateWithId{..} (curMax, res) ->
            if curMax < Just uId then (Just uId, uCont:res) else (curMax, uCont:res)) (Nothing, []) (idUpds :: [TelegramUpdateWithId])
    return ((+1) <$> lastUId, upds)

sendMes :: (MonadMask m, MonadIO m) => String -> L.Handle m -> TelegramMessageSend -> m ()
sendMes token hL tgMes = case tgMes & B.msContent of
    B.CGettable (GSticker _) -> void $ request token hL (Just tgMes) "sendSticker" []
    _   -> void $ request token hL (Just tgMes) "sendMessage" []

ansCb :: (MonadMask m, MonadIO m) => String -> L.Handle m -> Text -> B.CallbackQuery TelegramUserInfo -> m ()
ansCb token hL cbMes B.CallbackQuery {..} = void $ request token hL (Nothing :: Maybe String) "answerCallbackQuery"
    [("callback_query_id", pack cbId),
     ("text", cbMes)]
