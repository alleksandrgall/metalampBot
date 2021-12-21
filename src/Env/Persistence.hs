module Persistence where
import           Control.Concurrent.STM (TVar)
import           Data.Text              (Text)
import           Logger                 (LogLevel)

data Messenger = Tele | VK deriving (Show)
type Token = Text

data Persistance m = Persistance
    {
        logLevel     :: LogLevel,
        latestUpdate :: TVar Int
    }
