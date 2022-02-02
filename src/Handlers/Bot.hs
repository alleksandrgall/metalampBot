module Handlers.Bot where

import           Control.Concurrent.STM (TVar)
import           Data.Int               (Int64)
import           Data.Map               (Map)
import           Data.Text              (Text)
import           Handlers.Logger        as L
import           Handlers.Web           as Web

data Command = Help | Repeat

-- Может сделать класс с Type ????
data MessageContent = MCCommand Command | MCText Text | MCSticker Int64

data UpdateContent =
    UCCallbackQuary {
        cqChatId    :: Int64
      , cqFromId    :: Int64
      , cqMessageId :: Int64
      , cqData      :: String
    } |
    UCMessage {
        mChatId         :: Int64
      , mFromId         :: Int64
      , mMessageId      :: Int64
      , mMessageContent :: MessageContent
    }


