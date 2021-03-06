module Bot.Telegram.Implement (Config (..), parseConfig, withHandle) where

import Bot.Telegram.Types
import Config
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO ())
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BS
import Data.Function ((&))
import Data.Int (Int64)
import Data.Text (Text)
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
      cToken = appConfigTeleToken
    }

data Config = Config
  { cBaseRepeat :: Int,
    cStartMes :: Text,
    cHelpMes :: Text,
    cRepeatMes :: Text,
    cRepeatKeyboardMes :: Text,
    cToken :: Text
  }

withHandle ::
  (MonadCatch m, MonadIO m) =>
  Config ->
  L.Handle m ->
  (B.Handle TGGettable TGUserInfo m -> m a) ->
  m a
withHandle Config {..} hL f = do
  let h =
        B.Handle
          { hConfig = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes,
            hLogger = hL,
            hInit = handleWeb hL "initializing bot" 0 $ init cToken hL,
            hGetUpdates = handleWeb hL "getting updates" (Nothing, []) . getUpdates cToken hL,
            hSendMes = handleWeb hL "sending message" () . sendMes cToken hL,
            hAnswerCallback = \t cb -> handleWeb hL "answering callback" () $ ansCb cToken hL t cb
          }
  f h

request :: (MonadCatch m, MonadIO m, ToJSON b) => Text -> L.Handle m -> Maybe b -> Text -> [(Text, Text)] -> m BS.ByteString
request token hL body method = makeRequest hL body "api.telegram.org" ["bot" <> token, method]

init :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> m Int64
init token hL = do
  void $
    request
      token
      hL
      ( Just $
          Commands
            [ ("/repeat", "Use to change the repeat number."),
              ("/help", "Use to get help."),
              ("/start", "Use to see a start message")
            ]
      )
      "setMyCommands"
      []
  return 0

getUpdates :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> Int64 -> m (Maybe Int64, [TGUpdate])
getUpdates token hL offset = do
  TGResult idUpds <-
    parseResponse hL
      =<< request
        token
        hL
        (Nothing :: Maybe Text)
        "getUpdates"
        [ ("offset", showText offset),
          ("timeout", "2")
        ]
  let (lastUId, upds) =
        foldr
          ( \TGUpdateWithId {..} (curMax, res) ->
              if curMax < Just uId then (Just uId, uCont : res) else (curMax, uCont : res)
          )
          (Nothing, [])
          (idUpds :: [TGUpdateWithId])
  return ((+ 1) <$> lastUId, upds)

sendMes :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> TGMessageSend -> m ()
sendMes token hL tgMes = case tgMes & B.msContent of
  B.CGettable (GSticker _) -> void $ request token hL (Just tgMes) "sendSticker" []
  _ -> void $ request token hL (Just tgMes) "sendMessage" []

ansCb :: (MonadCatch m, MonadIO m) => Text -> L.Handle m -> Text -> B.CallbackQuery TGUserInfo -> m ()
ansCb token hL cbMes B.CallbackQuery {..} =
  void $
    request
      token
      hL
      (Nothing :: Maybe Text)
      "answerCallbackQuery"
      [ ("callback_query_id", cbId),
        ("text", cbMes)
      ]
