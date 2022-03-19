{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Handlers.Bot
  where

import           Control.Monad       (foldM, forever, replicateM_, unless)
import           Data.Foldable       (traverse_)
import           Data.Function       ((&))
import qualified Data.HashMap.Strict as HM
import           Data.Hashable       (Hashable)
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust, isNothing)
import           Data.String         (IsString (..))
import           Data.Text           (Text, unpack)
import qualified Handlers.Logger     as L
import           Text.Read           (readMaybe)

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

data Handle gettable usInf m = (Monad m, Eq usInf, Hashable usInf, Show usInf) => Handle {
    hConfig         :: Config
  , hLogger         :: L.Handle m
  , hInit           :: m Int64
  , hGetUpdates     :: Int64 -> m (Maybe Int64, [Update gettable usInf])
  , hSendMes        :: (IsString gettable) => MessageSend gettable usInf -> m ()
  , hAnswerCallback :: Text -> CallbackQuery usInf -> m ()
}

runBot :: (IsString gettable, Show usInf) =>
  Handle gettable usInf m -> m ()
runBot h@Handle {..} = do
  L.info hLogger "Initializing bot..."
  startingOffset <- hInit
  L.info hLogger "Bot has been initialized."
  botLoop h startingOffset (HM.fromList [])

botLoop :: (IsString gettable) => Handle gettable usInf m -> Int64 -> HM.HashMap usInf Int -> m ()
botLoop h@Handle {..} offset userRepeats = do
  (newOffset, uls) <- hGetUpdates offset
  unless (isNothing newOffset) $ do
    newUserRepeats <- processUpdates h userRepeats uls
    botLoop h (fromJust newOffset) newUserRepeats
  botLoop h offset userRepeats

processUpdates :: (IsString gettable) =>
  Handle gettable usInf m -> HM.HashMap usInf Int -> [Update gettable usInf] -> m (HM.HashMap usInf Int)
processUpdates h@Handle {..} userRepeats uls = do
  L.info hLogger "Processing updates..."
  newUserRepeats <- foldM processUpdateContent userRepeats uls
  L.info hLogger "Updates were processed."
  return newUserRepeats
    where
      processUpdateContent userRepeats upd = case upd of
        UCallbackQuary cb -> processCallback h cb userRepeats
        UCommand c        -> processCommand h c >> return userRepeats
        UMessage m        -> processMessage h m userRepeats >> return userRepeats
        UnknownUpdate     -> L.info hLogger "Got an unknown update." >> return userRepeats

processCallback :: Handle gettable usInf m -> CallbackQuery usInf -> HM.HashMap usInf Int -> m (HM.HashMap usInf Int)
processCallback Handle {..} cb@CallbackQuery {..} userRepeats = do
  L.info hLogger ("Processing callback from the " <> show cbUserInfo <> "...")
  let maybeNewRepeat = readMaybe cbData
      testNewRepeat = (`elem` [1..5]) <$> maybeNewRepeat
  if testNewRepeat == Just True then do
    let newRepeat = fromJust maybeNewRepeat
    hAnswerCallback (hConfig & cRepeatMes) cb
    L.info hLogger ("Repeat number of the " <> show cbUserInfo <> " was adjusted and answer was send to the user\n\t" <>
        "New number: " <> show newRepeat)
    return $ HM.insert cbUserInfo newRepeat userRepeats
  else L.warning hLogger ("Bad callback data from the " <> show cbUserInfo <> ", data: " <> fromString cbData) >>
       return userRepeats

processCommand :: (IsString gettable) => Handle gettable usInf m -> Command usInf -> m ()
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

processMessage :: (IsString gettable) => Handle gettable usInf m -> MessageGet gettable usInf -> HM.HashMap usInf Int -> m ()
processMessage Handle {..} MessageGet {..} userRepeats = do
  L.info hLogger $ "Processing message from the " <> show mgUserInfo
  userRepeat <- maybe (return (hConfig & cBaseRepeat)) return $ HM.lookup mgUserInfo userRepeats
  replicateM_ userRepeat $ hSendMes (MessageSend mgUserInfo (CGettable mgContent))
  L.info hLogger $ "Message was sent " <> show userRepeat <> " times to the " <> show mgUserInfo
