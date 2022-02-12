module Bot.Telegram.Implement where

import           Bot.Telegram.JSONInstances
import           Data.Text                  (Text)
import qualified Handlers.Bot               as B
import qualified Handlers.Logger            as L

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

withHandle :: Config -> L.Handle m -> (B.Handle m MessageText MessageSticker -> m a) -> m a
withHandle = undefined
