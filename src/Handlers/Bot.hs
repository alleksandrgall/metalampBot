{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Handlers.Bot
  (Config(..)
  ,Handle(..)
  ,runBot
  ,processUpdates
  ,processCallback
  ,processCommand
  ,processMessage
  ,UserInfo(..)
  ,MessageGet(..)
  ,MessageSend(..)
  ,SendContent(..)
  ,Command(..)
  ,CommandType(..)
  ,Update(..)
  ,UpdateContent(..)
  ,CallbackQuery(..)
  ,Keyboard(..)
  ) where

import           Control.Monad       (foldM, forever, replicateM_, unless, when)
import           Control.Monad.Catch (MonadCatch, MonadThrow (throwM), handle)
import           Data.Function       ((&))
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust, isNothing)
import           Data.String         (IsString (..))
import           Data.Text           (Text, pack, unpack)
import           GHC.Generics        (Generic)
import qualified Handlers.Logger     as L
import           Internal.Types      (Token)
import           Text.Read           (readMaybe)

data UserInfo = UserInfo {
    uiId     :: Int64
  , uiChatId :: Int64
} deriving (Generic, Show, Eq, Ord)

data MessageGet gettable = MessageGet {
    mgUserInfo  :: UserInfo
  , mgMessageId :: Int
  , mgContent   :: gettable
} deriving (Show, Eq)

data SendContent gettable = CGettable gettable | CKeyboard Text Keyboard
  deriving (Show, Eq)
data MessageSend gettable = MessageSend {
    msUserInfo :: UserInfo
  , msContent  :: SendContent gettable
} deriving (Show, Eq)

data UpdateContent gettable =
    UCCallbackQuary CallbackQuery |
    UCMessage (MessageGet gettable) |
    UCCommand Command |
    UnknownUpdate
    deriving (Show, Eq)

data CallbackQuery = CallbackQuery {
      cbUserInfo :: UserInfo
    , cbId       :: String
    , cbData     :: String
  } deriving (Show, Eq)

data Command = Command {
    cUserInfo    :: UserInfo
  , cCommandType :: CommandType
} deriving (Show, Eq)

data CommandType =
  Help |
  Repeat |
  Start
  deriving Eq

instance Show CommandType where
  show Help   = "/help"
  show Repeat = "/repeat"
  show Start  = "/start"

data Update gettable  = Update {
    uId     :: Int64
  , uUpdate :: UpdateContent gettable
  } deriving (Show, Eq)

newtype Keyboard = Keyboard [(Text, Text)] deriving (Show, Eq)

data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  }

-- | hInit, hGetUpdates, hSendMes, hAnsCB can throw a `Exceptions.Request.RequestException` or HttpException
data Handle gettable m = Handle {
    hConfig           :: Config
  , hLogger           :: L.Handle m
  , hInit             :: (Monad m) => m ()
  , hSleep            :: Int -> m ()
  , hGetUpdates       :: (Monad m) =>
      Int64 -> m [Update gettable]
  , hSendMes          :: (IsString gettable, Monad m) =>
      MessageSend gettable -> m ()
  -- | Notify the server and the user that button press was processed
  , hAnswerCallback   :: (Monad m) => Text -> CallbackQuery -> m ()
  -- | Getters and setters for offset and repeat counter for each user
  , hGetOffset        :: m Int64
  , hSetOffset        :: Int64 -> m ()
  , hInsertUserRepeat :: UserInfo -> Int -> m ()
  , hGetUserRepeat    :: UserInfo -> m (Maybe Int)
}

runBot :: (Monad m, IsString gettable) =>
  Handle gettable   m -> m ()
runBot h@Handle {..} = do
  L.info hLogger $ L.JustText "Initializing bot..."
  hInit
  L.info hLogger $ L.JustText "Bot has been initialized."
  forever (go h)
  where
    go h@Handle {..} = hGetOffset >>= hGetUpdates >>= (\us -> unless (null us)
      (L.info hLogger (L.JustText "Got updates.") >> processUpdates h us))

processUpdates :: (Monad m, IsString gettable) =>
  Handle gettable  m -> [Update gettable] -> m ()
processUpdates h@Handle {..} uls = do
  L.info hLogger $ L.JustText "Processing updates..."
  currentOffset <- hGetOffset
  newOffset <- foldM (\maxOffset (Update uId uc) -> do
    processUpdateContent h uc
    if uId >= maxOffset then return uId else return maxOffset) currentOffset uls
  L.info hLogger $ L.JustText "Updates were processed."
  hSetOffset (newOffset + 1)
    where
      processUpdateContent h@Handle {..} = \case
        UCCallbackQuary cb -> processCallback h cb
        UCCommand c        -> processCommand h c
        UCMessage m        -> processMessage h m
        UnknownUpdate      -> return ()

processCallback :: (Monad m) => Handle gettable m -> CallbackQuery  -> m ()
processCallback Handle {..} cb = do
  L.info hLogger $ L.JustText ("Processing callback from the " <> showUi (cb & cbUserInfo) <> "...")
  let mNewRepeat = ((readMaybe $ cb & cbData) :: Maybe Int)
  case mNewRepeat of
    Nothing -> L.warning hLogger $ L.JustText ("Bad callback from the " <> showUi (cb & cbUserInfo))
    Just newRepeat -> if newRepeat `notElem` [1..5] then
      L.warning hLogger $ L.JustText ("Bad callback from the " <> showUi (cb & cbUserInfo))
      else do
        hInsertUserRepeat (cb & cbUserInfo) newRepeat
        hAnswerCallback (hConfig & cRepeatMes) cb
        L.info hLogger $ L.JustText
          ("Repeat number of the " <> showUi (cb & cbUserInfo) <> " was adjusted and answer was send to the user\n\t" <>
           "New number: " <> (pack . show $ newRepeat))

processCommand :: (IsString gettable, Monad m) =>
  Handle gettable m -> Command  -> m ()
processCommand Handle {..} Command {..} = do
  L.info hLogger $ L.JustText ("Processing command " <> (pack . show $ cCommandType) <> " from the " <> showUi cUserInfo <> "...")
  case cCommandType of
    Start -> do
      hSendMes $ MessageSend cUserInfo (CGettable (fromString $ unpack (hConfig & cStartMes)))
      L.info hLogger $ L.JustText ("New user has been added, info:" <> showUi cUserInfo)
    Help -> do
      hSendMes $ MessageSend cUserInfo (CGettable (fromString $ unpack (hConfig & cHelpMes)))
      L.info hLogger $ L.JustText ("Help message was sent to the " <> showUi cUserInfo)
    Repeat -> do
      hSendMes $ MessageSend cUserInfo $ CKeyboard (hConfig & cRepeatKeyboardMes) repeatKeyboard
      L.info hLogger $ L.JustText ("Keyboard was sent to the " <> showUi cUserInfo)

processMessage :: (IsString gettable, Monad m) =>
  Handle gettable   m -> MessageGet gettable  -> m ()
processMessage Handle {..} MessageGet {..} = do
  L.info hLogger $ L.JustText ("Processing message from the " <> showUi mgUserInfo)
  userRepeat <- maybe (return (hConfig & cBaseRepeat)) return =<< hGetUserRepeat mgUserInfo
  replicateM_ userRepeat (hSendMes $ MessageSend mgUserInfo (CGettable mgContent))
  L.info hLogger $ L.JustText ("Message was sent " <> (pack . show $ userRepeat) <> " times to the " <> showUi mgUserInfo)

repeatKeyboard :: Keyboard
repeatKeyboard = Keyboard $ map (\i -> (pack . show $ i, pack . show $ i)) (take 5 ([1,2..] :: [Int]))
{-# INLINE repeatKeyboard #-}

showUi :: UserInfo -> Text
showUi UserInfo {..} = "user_id: " <> (pack . show $ uiId) <> ", chat_id: " <> (pack . show $ uiChatId)
