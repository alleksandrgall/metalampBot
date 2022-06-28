module Bot.VK.Implement where

import Bot.VK.Types
import Config
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as CBS (unpack)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Handlers.Bot as B
import qualified Handlers.Logger as L
import Internal.Req (makeRequest, parseResponse)
import Internal.ShowText (showText)
import Internal.Utils (handleWeb)
import Prelude hiding (init)

parseConfig :: AppConfig -> Config
parseConfig AppConfig {..} =
  Config
    { cBaseRepeat = appConfigRepeat & repeatDefaultNumber,
      cStartMes = appConfigStart & startMessage,
      cHelpMes = appConfigHelp & helpMessage,
      cRepeatMes = appConfigRepeat & repeatMessage,
      cRepeatKeyboardMes = appConfigRepeat & repeatKeyboardMes,
      cToken = appConfigVkToken,
      cGroupId = coerce appConfigGroupId
    }

data Config = Config
  { cBaseRepeat :: Int,
    cStartMes :: Text,
    cHelpMes :: Text,
    cRepeatMes :: Text,
    cRepeatKeyboardMes :: Text,
    cToken :: Text,
    cGroupId :: Int
  }

withHandle ::
  (MonadCatch m, MonadIO m) =>
  Config ->
  L.Handle m ->
  (B.Handle VKGettable VKUserInfo m -> m a) ->
  m a
withHandle Config {..} hL f = do
  key <- liftIO $ newIORef ""
  server <- liftIO $ newIORef ""
  let h =
        B.Handle
          { hConfig = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes,
            hLogger = hL,
            hInit = handleWeb hL "initializing bot" 0 $ init key server cToken cGroupId hL,
            hGetUpdates = handleWeb hL "getting updates" (Nothing, []) . getUpdates key server hL,
            hSendMes = handleWeb hL "sending message" () . sendMes cToken hL,
            hAnswerCallback = \t cb -> handleWeb hL "answering callback" () $ ansCb cToken hL t cb
          }
  f h

vkRequest :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> Text -> [(Text, Text)] -> m BS.ByteString
vkRequest token hL method params =
  makeRequest hL (Nothing :: Maybe Text) "api.vk.com" ["method", method] (("v", "5.131") : ("access_token", token) : params)

init :: (MonadCatch m, MonadIO m) => IORef Text -> IORef Text -> Text -> Int -> L.Handle m -> m Int64
init keyR serverR token groupId hL = do
  L.info hL "Setting up the group..."
  void $
    vkRequest
      token
      hL
      "groups.setLongPollSettings"
      [ ("group_id", showText groupId),
        ("enabled", "1"),
        ("message_event", "1"),
        ("message_new", "1")
      ]
  L.info hL "Intitializing LongPoll session..."
  res <- vkRequest token hL "groups.getLongPollServer" [("group_id", showText groupId)]
  initInfo <- parseResponse hL res
  liftIO $ writeIORef serverR (getServerPath . server $ initInfo)
  liftIO $ writeIORef keyR (key initInfo)
  return (initInfo & ts)
  where
    getServerPath = T.reverse . T.takeWhile (/= '/') . T.reverse

getUpdates :: (MonadCatch m, MonadIO m) => IORef Text -> IORef Text -> L.Handle m -> Int64 -> m (Maybe Int64, [VKUpdate])
getUpdates keyR serverR hL offset = do
  server <- liftIO $ readIORef serverR
  key <- liftIO $ readIORef keyR
  updRes <-
    parseResponse hL
      =<< makeRequest
        hL
        (Nothing :: Maybe Text)
        "lp.vk.com"
        [server]
        [ ("act", "a_check"),
          ("key", key),
          ("ts", showText offset),
          ("wait", "20")
        ]
  if (updRes & newTs) == offset
    then return (Nothing, updRes & updates)
    else return (Just (updRes & newTs), updRes & updates)

sendMes :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> VKMessageSend -> m ()
sendMes token hL B.MessageSend {..} =
  void $
    vkRequest token hL "messages.send" $
      [buildUserInfo msUserInfo, ("random_id", "0")] ++ messageInfo
  where
    buildUserInfo VKUserInfo {..} =
      if uiId == uiPeerId
        then ("user_id", showText uiId)
        else ("chat_id", showText $ uiPeerId - 2000000000)
    messageInfo = case msContent of
      B.CGettable (GText txt) -> [("message", txt)]
      B.CGettable (GSticker id_) -> [("sticker_id", showText id_)]
      B.CKeyboard txt -> [("message", txt), ("keyboard", T.pack . CBS.unpack . encode $ VKKeyboard)]

ansCb :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> Text -> B.CallbackQuery VKUserInfo -> m ()
ansCb token hL repeatMessage B.CallbackQuery {..} =
  void $
    vkRequest
      token
      hL
      "messages.sendMessageEventAnswer"
      [ ("event_id", cbId),
        ("user_id", showText $ cbUserInfo & uiId),
        ("peer_id", showText $ cbUserInfo & uiPeerId),
        ("event_data", T.pack . CBS.unpack . encode $ SnackBar repeatMessage)
      ]
