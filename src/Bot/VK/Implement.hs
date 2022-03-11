
module Bot.VK.Implement where
import           Bot.VK.Types                 (SnackBar (SnackBar),
                                               VKGettable (GSticker, GText),
                                               VKKeyboard (VKKeyboard),
                                               VKMessageSend, VKUpdate,
                                               VKUpdateResult (newTs, updates),
                                               VKUserInfo (..))
import           Config
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Aeson                   (FromJSON (parseJSON),
                                               Value (Object), defaultOptions,
                                               encode, genericParseJSON, (.:))
import qualified Data.ByteString.Lazy         as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8   as CBS (unpack)
import           Data.Coerce                  (coerce)
import           Data.Function                ((&))
import           Data.Functor                 (void, (<&>))
import qualified Data.HashMap.Internal.Strict as HM (HashMap, insert, lookup)
import           Data.IORef                   (IORef, modifyIORef', newIORef,
                                               readIORef, writeIORef)
import           Data.Int                     (Int64)
import           Data.String                  (IsString (fromString))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import qualified Handlers.Bot                 as B
import qualified Handlers.Logger              as L
import           Internal.Req                 (makeRequest, parseResponse)
import           Prelude                      hiding (init)


parseConfig :: AppConfig -> Config
parseConfig AppConfig {..} = Config {
    cBaseRepeat        = appConfigRepeat & repeatDefaultNumber
  , cStartMes          = appConfigStart & startMessage
  , cHelpMes           = appConfigHelp & helpMessage
  , cRepeatMes         = appConfigRepeat & repeatMessage
  , cRepeatKeyboardMes = appConfigRepeat & repeatKeyboardMes
  , cToken             = appConfigToken
  , cGroupId           = coerce appConfigGroupId
}
data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  , cToken             :: String
  , cGroupId           :: Int
}

withHandle :: (MonadCatch m, MonadIO m) =>
    Config -> L.Handle m -> (B.Handle VKGettable VKUserInfo m -> m a) -> m a
withHandle Config {..} hL f = do
    offset <- liftIO $ newIORef 0
    userRepeat <- liftIO $ newIORef (mempty :: HM.HashMap VKUserInfo Int)
    key <- liftIO $ newIORef ""
    server <- liftIO $ newIORef ""
    let h = B.Handle {
          hConfig             = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes
        , hLogger             = hL
        , hInit               = init offset key server cToken cGroupId hL
        , hGetUpdates         = getUpdates key server hL
        , hSendMes            = sendMessage cToken hL
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


vkRequest :: (MonadCatch m, MonadIO m) => String -> L.Handle m  -> Text -> [(Text, Text)] -> m BS.ByteString
vkRequest token hL method params =
  makeRequest hL (Nothing :: Maybe String) "api.vk.com" ["method", method] (("v", "5.131"):("access_token", fromString token):params)

data GetLongPollAnswer = GetLongPollAnswer {
  key    :: String,
  server :: String,
  ts     :: String
} deriving (Show, Generic)
instance FromJSON GetLongPollAnswer where
  parseJSON (Object o) = o .: "response" >>= genericParseJSON defaultOptions
  parseJSON _          = mempty

init :: (MonadCatch m, MonadIO m)  => IORef Int64 -> IORef String -> IORef String -> String -> Int -> L.Handle m -> m ()
init offsetR keyR serverR token groupId hL = do
  res <- vkRequest token hL "groups.getLongPollServer" [("group_id", fromString . show $ groupId)]
  initInfo <- parseResponse hL res
  liftIO $ writeIORef offsetR (read $ ts initInfo)
  liftIO $ writeIORef serverR (getServerPath . server $ initInfo)
  liftIO $ writeIORef keyR (key initInfo)
  where getServerPath = reverse . takeWhile (/='/') . reverse

getUpdates :: (MonadCatch m, MonadIO m) => IORef String -> IORef String -> L.Handle m -> Int64 -> m (Maybe Int64, [VKUpdate])
getUpdates keyR serverR hL offset = do
  server <- liftIO $ readIORef serverR
  key    <- liftIO $ readIORef keyR
  updRes <- parseResponse hL =<< makeRequest hL (Nothing :: Maybe String) "lp.vk.com" [fromString server]
    [
      ("act", "a_check"),
      ("key", fromString key),
      ("ts", fromString . show $ offset),
      ("wait", "20")
    ]
  if (updRes & newTs) == offset then return (Nothing, updRes & updates)
    else return (Just (updRes & newTs), updRes & updates)

sendMessage :: (MonadCatch m, MonadIO m)  => String -> L.Handle m -> VKMessageSend -> m ()
sendMessage token hL B.MessageSend {..} = void $ vkRequest token hL "messages.send" $
  [buildUserInfo msUserInfo, ("random_id", "0")] ++ messageInfo
  where
    buildUserInfo VKUserInfo {..} = if uiId == uiPeerId then ("user_id", fromString . show $ uiId)
      else ("chat_id", fromString . show $ (uiPeerId - 2000000000))
    messageInfo = case msContent of
          B.CGettable (GText txt)     -> [("message", fromString txt)]
          B.CGettable (GSticker id_)  -> [("sticker_id", fromString . show $ id_)]
          B.CKeyboard txt             -> [("message", txt), ("keyboard", pack . CBS.unpack . encode $ VKKeyboard)]

ansCb :: (MonadCatch m, MonadIO m)  => String -> L.Handle m -> Text -> B.CallbackQuery VKUserInfo -> m ()
ansCb token hL repeatMessage B.CallbackQuery {..} = void $ vkRequest token hL "messages.sendMessageEventAnswer"
  [("event_id", pack cbId),
   ("user_id" , pack .show $ cbUserInfo & uiId),
   ("peer_id" , pack . show $ cbUserInfo & uiPeerId),
   ("event_data", pack . CBS.unpack . encode $ SnackBar repeatMessage)]
