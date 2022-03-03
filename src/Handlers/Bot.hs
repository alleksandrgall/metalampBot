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
  ,Keyboard(..)
  ) where

import           Control.Monad       (foldM, forever, replicateM_, unless, when)
import           Control.Monad.Catch (MonadCatch, MonadThrow (throwM), handle)
import           Data.Foldable       (traverse_)
import           Data.Function       ((&))
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust, isNothing)
import           Data.String         (IsString (..))
import           Data.Text           (Text, pack, unpack)
import           GHC.Generics        (Generic)
import qualified Handlers.Logger     as L
import           Internal.Types      (Token)
import           Text.Read           (readMaybe)

data MessageGet gettable usInf = MessageGet {
    mgUserInfo :: usInf
  , mgContent  :: gettable
} deriving (Show, Eq)

data SendContent gettable = CGettable gettable | CKeyboard Text Keyboard
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

newtype Keyboard = Keyboard [Text] deriving (Show, Eq)

data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  }

-- | hInit, hGetUpdates, hSendMes, hAnsCB can throw a `Exceptions.Request.RequestException` or HttpException
data Handle gettable usInf m = Handle {
    hConfig           :: Config
  , hLogger           :: L.Handle m
  , hInit             :: (Monad m) => m ()
  , hGetUpdates       :: (Monad m) =>
      Int64 -> m (Maybe Int64, [Update gettable usInf])
  , hSendMes          :: (IsString gettable, Monad m) =>
      MessageSend gettable usInf -> m ()
  -- | Notify the server and the user that button press was processed
  , hAnswerCallback   :: (Monad m) => Text -> CallbackQuery usInf -> m ()
  -- | Getters and setters for offset and repeat counter for each user
  , hGetOffset        :: m Int64
  , hSetOffset        :: Int64 -> m ()
  , hInsertUserRepeat :: usInf -> Int -> m ()
  , hGetUserRepeat    :: usInf -> m (Maybe Int)
}

runBot :: (Monad m, IsString gettable, Show usInf) =>
  Handle gettable usInf m -> m ()
runBot h@Handle {..} = do
  L.info hLogger $ L.JustText "Initializing bot..."
  hInit
  L.info hLogger $ L.JustText "Bot has been initialized."
  forever (go h)
  where
    go h@Handle {..} = hGetOffset >>= hGetUpdates >>= (\(newOffset, uls) -> unless (isNothing newOffset)
      (L.info hLogger (L.JustText "Got updates.") >> processUpdates h newOffset uls))

processUpdates :: (Monad m, IsString gettable, Show usInf) =>
  Handle gettable usInf m -> Maybe Int64 -> [Update gettable usInf] -> m ()
processUpdates h@Handle {..} newOffset uls = do
  L.info hLogger $ L.JustText "Processing updates..."
  traverse_ (processUpdateContent h) uls
  L.info hLogger $ L.JustText "Updates were processed."
  maybe (return ()) hSetOffset newOffset
    where
      processUpdateContent h@Handle {..} = \case
        UCallbackQuary cb -> processCallback h cb
        UCommand c        -> processCommand h c
        UMessage m        -> processMessage h m
        UnknownUpdate     -> L.info hLogger $ L.JustText "Got an unknown update."

processCallback :: (Monad m, Show usInf) => Handle gettable usInf m -> CallbackQuery usInf -> m ()
processCallback Handle {..} cb@CallbackQuery {..} = do
  L.info hLogger $ L.JustText ("Processing callback from the " <> (fromString .show $ cbUserInfo) <> "...")
  let maybeNewRepeat = readMaybe cbData
      testNewRepeat = (`elem` [1..5]) <$> maybeNewRepeat
  if isNothing testNewRepeat || testNewRepeat == Just False then
    L.warning hLogger $ L.JustText
      ("Bad callback data from the " <> (fromString .show $ cbUserInfo) <> ", data: " <> fromString cbData)
  else do
    let newRepeat = fromJust maybeNewRepeat
    hInsertUserRepeat cbUserInfo newRepeat
    hAnswerCallback (hConfig & cRepeatMes) cb
    L.info hLogger $ L.JustText
      ("Repeat number of the " <> (fromString .show $ cbUserInfo) <> " was adjusted and answer was send to the user\n\t" <>
        "New number: " <> (fromString . show $ newRepeat))

processCommand :: (IsString gettable, Monad m, Show usInf) =>
  Handle gettable usInf m -> Command usInf -> m ()
processCommand Handle {..} Command {..} = do
  L.info hLogger $ L.JustText ("Processing command " <> (fromString .show $ cCommandType) <>
    " from the " <> (fromString .show $ cUserInfo) <> "...")
  case cCommandType of
    Start -> do
      hSendMes $ MessageSend cUserInfo (fromString . unpack $ hConfig & cStartMes)
      L.info hLogger $ L.JustText ("New user has been added, info:" <> (fromString .show $ cUserInfo))
    Help -> do
      hSendMes $ MessageSend cUserInfo (fromString . unpack $ hConfig & cHelpMes)
      L.info hLogger $ L.JustText ("Help message was sent to the " <> (fromString .show $ cUserInfo))
    Repeat -> do
      hSendMes $ MessageSend cUserInfo $ CKeyboard (hConfig & cRepeatKeyboardMes) repeatKeyboard
      L.info hLogger $ L.JustText ("Keyboard was sent to the " <> (fromString .show $ cUserInfo))

processMessage :: (IsString gettable, Monad m, Show usInf) =>
  Handle gettable usInf m -> MessageGet gettable usInf -> m ()
processMessage Handle {..} MessageGet {..} = do
  L.info hLogger $ L.JustText ("Processing message from the " <> (fromString .show $ mgUserInfo))
  userRepeat <- maybe (return (hConfig & cBaseRepeat)) return =<< hGetUserRepeat mgUserInfo
  replicateM_ userRepeat (hSendMes $ MessageSend mgUserInfo (CGettable mgContent))
  L.info hLogger $ L.JustText ("Message was sent " <> (fromString .show $ userRepeat) <> " times to the " <> (fromString .show $ mgUserInfo))

repeatKeyboard :: Keyboard
repeatKeyboard = Keyboard ["1", "2", "3", "4", "5"]
{-# INLINE repeatKeyboard #-}
