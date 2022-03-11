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
  ,MessageGet(..)
  ,MessageSend(..)
  ,SendContent(..)
  ,Command(..)
  ,CommandType(..)
  ,Update(..)
  ,CallbackQuery(..)
  ) where

import           Control.Monad   (forever, replicateM_, unless)
import           Data.Foldable   (traverse_)
import           Data.Function   ((&))
import           Data.Int        (Int64)
import           Data.Maybe      (fromJust, isNothing)
import           Data.String     (IsString (..))
import           Data.Text       (Text, unpack)
import qualified Handlers.Logger as L
import           Text.Read       (readMaybe)

data MessageGet gettable usInf = MessageGet {
    mgUserInfo :: usInf
  , mgContent  :: gettable
} deriving (Show, Eq)

data SendContent gettable = CGettable gettable | CKeyboard Text
  deriving (Show, Eq)

instance (IsString gettable) => IsString (SendContent gettable) where
  fromString s = CGettable . fromString $ s
data MessageSend gettable usInf = MessageSend {
    msUserInfo :: usInf
  , msContent  :: SendContent gettable
} deriving (Show, Eq)

data CallbackQuery usInf = CallbackQuery {
      cbUserInfo :: usInf
    , cbId       :: String
    , cbData     :: String
  } deriving (Show, Eq)

data Command usInf = Command {
    cUserInfo    :: usInf
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

data Update gettable usInf =
    UCallbackQuary (CallbackQuery usInf) |
    UMessage (MessageGet gettable usInf) |
    UCommand (Command usInf) |
    UnknownUpdate
    deriving (Show, Eq)

data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  }

-- | hInit, hGetUpdates, hSendMes, hAnsCB can throw a `Exceptions.Request.RequestException` or HttpException
data Handle gettable usInf m = (Monad m) => Handle {
    hConfig           :: Config
  , hLogger           :: L.Handle m
  , hInit             :: m ()
  , hGetUpdates       :: Int64 -> m (Maybe Int64, [Update gettable usInf])
  , hSendMes          :: (IsString gettable) =>
      MessageSend gettable usInf -> m ()
  -- | Notify the server and the user that button press was processed
  , hAnswerCallback   :: Text -> CallbackQuery usInf -> m ()
  -- | Getters and setters for offset and repeat counter for each user
  , hGetOffset        :: m Int64
  , hSetOffset        :: Int64 -> m ()
  , hInsertUserRepeat :: usInf -> Int -> m ()
  , hGetUserRepeat    :: usInf -> m (Maybe Int)
}

runBot :: (Monad m, IsString gettable, Show usInf) =>
  Handle gettable usInf m -> m ()
runBot h@Handle {..} = do
  L.info hLogger "Initializing bot..."
  hInit
  L.info hLogger "Bot has been initialized."
  forever go
  where
    go = hGetOffset >>= hGetUpdates >>= (\(newOffset, uls) -> unless (isNothing newOffset)
      (L.info hLogger "Got updates." >> processUpdates h newOffset uls))

processUpdates :: (Monad m, IsString gettable, Show usInf) =>
  Handle gettable usInf m -> Maybe Int64 -> [Update gettable usInf] -> m ()
processUpdates h@Handle {..} newOffset uls = do
  L.info hLogger "Processing updates..."
  traverse_ processUpdateContent uls
  L.info hLogger "Updates were processed."
  maybe (return ()) hSetOffset newOffset
    where
      processUpdateContent = \case
        UCallbackQuary cb -> processCallback h cb
        UCommand c        -> processCommand h c
        UMessage m        -> processMessage h m
        UnknownUpdate     -> L.info hLogger "Got an unknown update."

processCallback :: (Monad m, Show usInf) => Handle gettable usInf m -> CallbackQuery usInf -> m ()
processCallback Handle {..} cb@CallbackQuery {..} = do
  L.info hLogger ("Processing callback from the " <> show cbUserInfo <> "...")
  let maybeNewRepeat = readMaybe cbData
      testNewRepeat = (`elem` [1..5]) <$> maybeNewRepeat
  if isNothing testNewRepeat || testNewRepeat == Just False then
    L.warning hLogger ("Bad callback data from the " <> show cbUserInfo <> ", data: " <> fromString cbData)
  else do
    let newRepeat = fromJust maybeNewRepeat
    hInsertUserRepeat cbUserInfo newRepeat
    hAnswerCallback (hConfig & cRepeatMes) cb
    L.info hLogger ("Repeat number of the " <> show cbUserInfo <> " was adjusted and answer was send to the user\n\t" <>
        "New number: " <> show newRepeat)

processCommand :: (IsString gettable, Monad m, Show usInf) =>
  Handle gettable usInf m -> Command usInf -> m ()
processCommand Handle {..} Command {..} = do
  L.info hLogger ("Processing command " <> show cCommandType <> " from the " <> show cUserInfo <> "...")
  case cCommandType of
    Start -> do
      hSendMes $ MessageSend cUserInfo (fromString . unpack $ hConfig & cStartMes)
      L.info hLogger ("New user has been added, info:" <> show cUserInfo)
    Help -> do
      hSendMes $ MessageSend cUserInfo (fromString . unpack $ hConfig & cHelpMes)
      L.info hLogger ("Help message was sent to the " <> show cUserInfo)
    Repeat -> do
      hSendMes $ MessageSend cUserInfo $ CKeyboard (hConfig & cRepeatKeyboardMes)
      L.info hLogger ("Keyboard was sent to the " <> show cUserInfo)

processMessage :: (IsString gettable, Monad m, Show usInf) =>
  Handle gettable usInf m -> MessageGet gettable usInf -> m ()
processMessage Handle {..} MessageGet {..} = do
  L.info hLogger $ "Processing message from the " <> show mgUserInfo
  userRepeat <- maybe (return (hConfig & cBaseRepeat)) return =<< hGetUserRepeat mgUserInfo
  replicateM_ userRepeat (hSendMes $ MessageSend mgUserInfo (CGettable mgContent))
  L.info hLogger $ "Message was sent " <> show userRepeat <> " times to the " <> show mgUserInfo
