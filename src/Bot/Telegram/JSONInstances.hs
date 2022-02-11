module Bot.Telegram.JSONInstances where

import           Data.Aeson
import           Data.String  (IsString (fromString))
import           Handlers.Bot

data MessageText = MessageText {
      mText     :: String
    , mEntities :: [String]
}
instance IsString MessageText where
    fromString s = MessageText s []


newtype MessageSticker = MessageSticker {
    mSticker :: String
}

