module Handlers.Bot where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Catch    (MonadThrow)
import           Data.Int               (Int64)
import           Data.Map               (Map)
import           Data.Text              (Text)
import qualified Handlers.Logger        as L
import qualified Handlers.Web           as Web
import           Internal.Types         (Token)

data Command = Help | Repeat

-- Может сделать класс с Type ????
data MessageContent = MCCommand Command | MCText Text | MCSticker Int64 | MCFile Int64

data UpdateContent =
    CallbackQuary {
        cqChatId    :: Int64
      , cqFromId    :: Int64
      , cqMessageId :: Int64
      , cqData      :: String
    } |
    Message {
        mChatId         :: Int64
      , mFromId         :: Int64
      , mMessageId      :: Int64
      , mMessageContent :: MessageContent
    }

data Update = Update {
        uId     :: Int64
      , uUpdate :: [UpdateContent]
    }

data Config = Config {
    cToken      :: Token
  , cBaseRepeat :: Int
}

data Handle m a b = Handle {
    hConfig     :: Config
  , hWeb        :: Web.Handle m a b
  , hLogger     :: L.Handle m
  , hUserRepeat :: TVar (Map Int64 Int)
  , hOffset     :: TVar Int64
}

class (MonadThrow m) => Bot m where
  init  :: Handle m a b -> m ()
  getUpdates :: Handle m a b -> m [UpdateContent]
  processUpdates :: Handle m a b -> [UpdateContent] -> m ()

