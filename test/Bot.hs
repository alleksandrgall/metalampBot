module Bot where

import           Data.Aeson
import           Handlers.Bot
import qualified Handlers.Logger as L

data Gettable = GText String | GSticker Int
    deriving (Show)

type UserInfo = Int

mockHandle :: Handle Gettable UserInfo m
mockHandle = Handle {
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
        , hShowUserInfo       = undefined
}
