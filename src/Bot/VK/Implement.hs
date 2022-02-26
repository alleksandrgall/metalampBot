module Bot.VK.Implement where
import           Config
import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Function                ((&))
import qualified Data.HashMap.Internal.Strict as HM
import           Data.IORef                   (newIORef, readIORef, writeIORef)
import           Data.Text                    (Text)
import qualified Handlers.Bot                 as B
import qualified Handlers.Logger              as L


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

-- withHandle :: (MonadMask m, MonadIO m) =>
--     Config -> L.Handle m -> (B.Handle ... m -> m a) -> m a
withHandle Config {..} hL f = do
    offset <- liftIO $ newIORef 0
    --userRepeat <- liftIO $ newIORef (mempty :: HM.HashMap B.UserInfo Int)
    key <- liftIO $ newIORef ""
    server <- liftIO $ newIORef ""
    let c = B.Config cBaseRepeat cStartMes cHelpMes cRepeatMes cRepeatKeyboardMes
        h = B.Handle {
          hConfig             = undefined
        , hLogger             = undefined
        , hInit               = undefined
        , hSleep              = undefined
        , hGetUpdates         = undefined
        , hSendMes            = undefined
        , hAnswerCallback     = undefined
        , hGetOffset          = undefined
        , hSetOffset          = undefined
        , hInsertUserRepeat   = undefined
        , hGetUserRepeat      = undefined
    }
    f h

