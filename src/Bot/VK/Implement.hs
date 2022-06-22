
module Bot.VK.Implement where
import           Bot.VK.Types
import           Config
import           Control.Monad                (void)
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Aeson                   (encode)
import qualified Data.ByteString.Lazy         as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8   as CBS (unpack)
import           Data.Coerce                  (coerce)
import           Data.Function                ((&))
import qualified Data.HashMap.Internal.Strict as HM (HashMap, insert, lookup)
import           Data.IORef
import           Data.Int                     (Int64)
import           Data.String                  (IsString (fromString))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import qualified Handlers.Bot                 as B
import qualified Handlers.Logger              as L
import           Internal.Req                 (makeRequest, parseResponse)
import           Internal.Utils               (handleWeb)
import           Prelude                      hiding (init)


parseConfig :: AppConfig -> Config
parseConfig AppConfig {..} = Config {
    cBaseRepeat        = appConfigRepeat & repeatDefaultNumber
  , cStartMes          = appConfigStart & startMessage
  , cHelpMes           = appConfigHelp & helpMessage
  , cRepeatMes         = appConfigRepeat & repeatMessage
  , cRepeatKeyboardMes = appConfigRepeat & repeatKeyboardMes
  , cToken             = appConfigVkToken
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
    key <- liftIO $ newIORef ""
    server <- liftIO $ newIORef ""
    let h = B.Handle {
          hConfig           = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes
        , hLogger           = hL
        , hInit             = handleWeb hL "initializing bot" 0 $ init key server cToken cGroupId hL
        , hGetUpdates       = handleWeb hL "getting updates" (Nothing, []) . getUpdates key server hL
        , hSendMes          = handleWeb hL "sending message" () . sendMes cToken hL
        , hAnswerCallback   = \t cb -> handleWeb hL "answering callback" () $ ansCb cToken hL t cb
    }
    f h
      where
      getUserRepeat ref ui = fmap (HM.lookup ui) (liftIO . readIORef $ ref)
      insertUserRepeat ref ui r = liftIO $ modifyIORef' ref (HM.insert ui r)


vkRequest :: (MonadCatch m, MonadIO m) => String -> L.Handle m  -> Text -> [(Text, Text)] -> m BS.ByteString
vkRequest token hL method params =
  makeRequest hL (Nothing :: Maybe String) "api.vk.com" ["method", method] (("v", "5.131"):("access_token", fromString token):params)

init :: (MonadCatch m, MonadIO m)  => IORef String -> IORef String -> String -> Int -> L.Handle m -> m Int64
init keyR serverR token groupId hL = do
  res <- vkRequest token hL "groups.getLongPollServer" [("group_id", fromString . show $ groupId)]
  initInfo <- parseResponse hL res
  liftIO $ writeIORef serverR (getServerPath . server $ initInfo)
  liftIO $ writeIORef keyR (key initInfo)
  return (read $ initInfo & ts)
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

sendMes :: (MonadCatch m, MonadIO m)  => String -> L.Handle m -> VKMessageSend -> m ()
sendMes token hL B.MessageSend {..} = void $ vkRequest token hL "messages.send" $
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
