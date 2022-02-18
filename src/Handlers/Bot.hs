{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
-- | TODO Get rid of FromJSON instances in logic. Return simple User data type rather then parametrized one
module Handlers.Bot
  (Config(..)
  ,Handle(..)
  ,runBot
  ,getUpdates
  ,processUpdates
  ,processCallback
  ,processCommand
  ,processMessage
  ,processUpdateContent
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
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Function       ((&))
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust, isNothing)
import           Data.String         (IsString (..))
import           Data.Text           (Text, pack, unpack)
import           Exceptions.Request
import qualified Handlers.Logger     as L
import           Internal.Types      (Token)
import           Text.Read           (readMaybe)

type UserInfo = Int64

data MessageGet gettable userInfo = MessageGet {
    mgUserInfo  :: userInfo
  , mgMessageId :: Int
  , mgContent   :: gettable
} deriving Show

data SendContent gettable = CGettable gettable | CKeyboard Text Keyboard
  deriving Show
data MessageSend gettable userInfo = MessageSend {
    msUserInfo :: userInfo
  , msContent  :: SendContent gettable
} deriving Show

data UpdateContent gettable userInfo =
    UCCallbackQuary (CallbackQuery userInfo) |
    UCMessage (MessageGet gettable userInfo) |
    UCCommand (Command userInfo) |
    UnknownUpdate
    deriving (Show)

data CallbackQuery userInfo = CallbackQuery {
      cbUserInfo :: userInfo
    , cbId       :: String
    , cbData     :: String
  } deriving (Show)

data Command userInfo = Command {
    cUserInfo    :: userInfo
  , cCommandType :: CommandType
} deriving Show

data CommandType =
  Help |
  Repeat |
  Start

instance Show CommandType where
  show Help   = "/help"
  show Repeat = "/repeat"
  show Start  = "/start"

data Update gettable userInfo  = Update {
    uId     :: Int64
  , uUpdate :: UpdateContent gettable userInfo
  } deriving (Show)

newtype Keyboard = Keyboard [(Text, Text)] deriving Show

data Config = Config {
    cBaseRepeat        :: Int
  , cStartMes          :: Text
  , cHelpMes           :: Text
  , cRepeatMes         :: Text
  , cRepeatKeyboardMes :: Text
  -- | Delay in microsec
  , cDelay             :: Int
  }

-- | hInit, hGetUpdates, hSendMes, hAnsCB can throw a `Exceptions.Request.RequestException`
data Handle gettable userInfo  m = Handle {
    hConfig           :: Config
  , hLogger           :: L.Handle m
  , hInit             :: (MonadCatch m) => m ()
  , hSleep            :: m ()
  , hGetUpdates       :: (FromJSON (Update gettable userInfo), MonadCatch m) =>
      Int64 -> m [Update gettable userInfo]
  , hSendMes          :: (ToJSON (MessageSend gettable userInfo), IsString gettable, MonadCatch m) =>
      MessageSend gettable userInfo -> m ()
  -- | Notify the server and the user that button press was processed
  , hAnswerCallback   :: (MonadCatch m) => Text -> CallbackQuery userInfo -> m ()
  -- | Getters and setters for offset and repeat counter for each user
  , hGetOffset        :: m Int64
  , hSetOffset        :: Int64 -> m ()
  , hInsertUserRepeat :: userInfo -> Int -> m ()
  , hGetUserRepeat    :: userInfo -> m (Maybe Int)
  -- | For logging, keep it short and preferably in one line
  , hShowUserInfo     :: userInfo -> Text
  }

runBot :: (MonadCatch m, FromJSON (Update gettable userInfo), IsString gettable, ToJSON (MessageSend gettable userInfo)) =>
  Handle gettable userInfo  m -> m ()
runBot h@Handle {..} = do
  L.info hLogger $ L.JustText "Initializing bot..."
  hInit
  L.info hLogger $ L.JustText "Bot Initialized."
  forever (go h)
  where
    go h = getUpdates h >>= (\us -> unless (null us)
      (L.info hLogger (L.JustText "Got updates.") >> processUpdates h us))

getUpdates :: (FromJSON (Update gettable userInfo), MonadCatch m) =>
  Handle gettable userInfo m -> m [Update gettable userInfo]
getUpdates Handle {..} = hGetOffset >>= hGetUpdates

processUpdates :: (MonadCatch m, IsString gettable, ToJSON (MessageSend gettable userInfo)) =>
  Handle gettable userInfo m -> [Update gettable userInfo] -> m ()
processUpdates h@Handle {..} uls = do
  L.info hLogger $ L.JustText "Processing updates..."
  currentOffset <- hGetOffset
  newOffset <- foldM (\maxOffset (Update uId uc) -> do
    when (uId >= currentOffset) (processUpdateContent h uc)
    if uId >= maxOffset then return uId else return maxOffset) currentOffset uls
  L.info hLogger $ L.JustText "Updates was processed."
  hSetOffset (newOffset + 1)

processUpdateContent :: (MonadCatch m, IsString gettable, ToJSON (MessageSend gettable userInfo)) =>
  Handle gettable userInfo m -> UpdateContent gettable userInfo -> m ()
processUpdateContent h@Handle {..} = \case
  UCCallbackQuary cb -> processCallback h cb
  UCCommand c        -> processCommand h c
  UCMessage m        -> processMessage h m
  UnknownUpdate      -> return ()

processCallback :: (MonadCatch m) => Handle gettable userInfo m -> CallbackQuery userInfo -> m ()
processCallback Handle {..} cb = do
  L.info hLogger $ L.JustText ("Processing callback from the " <> hShowUserInfo (cb & cbUserInfo) <> "...")
  let mNewRepeat = ((readMaybe $ cb & cbData) :: Maybe Int)
  case mNewRepeat of
    Nothing -> L.warning hLogger $ L.JustText ("Bad callback from the " <> hShowUserInfo (cb & cbUserInfo))
    Just newRepeat -> if newRepeat `notElem` [1..5] then
      L.warning hLogger $ L.JustText ("Bad callback from the " <> hShowUserInfo (cb & cbUserInfo))
      else do
        hInsertUserRepeat (cb & cbUserInfo) newRepeat
        hAnswerCallback (hConfig & cRepeatMes) cb
        L.info hLogger $ L.JustText
          ("Repeat number of the " <> hShowUserInfo (cb & cbUserInfo) <> " was adjusted and answer was send to the user\n\t" <>
           "New number: " <> (pack . show $ newRepeat))

processCommand :: (IsString gettable, MonadCatch m, ToJSON (MessageSend gettable userInfo)) =>
  Handle gettable userInfo  m -> Command userInfo -> m ()
processCommand Handle {..} Command {..} = do
  L.info hLogger $ L.JustText ("Processing command " <> (pack . show $ cCommandType) <> " from the " <> hShowUserInfo cUserInfo <> "...")
  case cCommandType of
    Start -> do
      hGetUserRepeat cUserInfo >>= maybe (hInsertUserRepeat cUserInfo (hConfig & cBaseRepeat)) (\_ -> return ())
      hSendMes $ MessageSend cUserInfo (CGettable (fromString $ unpack (hConfig & cStartMes)))
      L.info hLogger $ L.JustText ("New user has been added, info:" <> hShowUserInfo cUserInfo)
    Help -> do
      hSendMes $ MessageSend cUserInfo (CGettable (fromString $ unpack (hConfig & cHelpMes)))
      L.info hLogger $ L.JustText ("Help message was sent to the " <> hShowUserInfo cUserInfo)
    Repeat -> do
      hSendMes $ MessageSend cUserInfo $ CKeyboard (hConfig & cRepeatKeyboardMes) repeatKeyboard
      L.info hLogger $ L.JustText ("Keyboard was sent to the " <> hShowUserInfo cUserInfo)

processMessage :: (IsString gettable, ToJSON (MessageSend gettable userInfo), MonadCatch m) =>
  Handle gettable userInfo  m -> MessageGet gettable userInfo -> m ()
processMessage Handle {..} MessageGet {..} = do
  L.info hLogger $ L.JustText ("Processing message from the " <> hShowUserInfo mgUserInfo)
  userRepeat <- maybe (return (hConfig & cBaseRepeat)) return =<< hGetUserRepeat mgUserInfo
  replicateM_ userRepeat (hSendMes $ MessageSend mgUserInfo (CGettable mgContent))
  L.info hLogger $ L.JustText ("Message was sent " <> (pack . show $ userRepeat) <> " times to the " <> hShowUserInfo mgUserInfo)

repeatKeyboard :: Keyboard
repeatKeyboard = Keyboard $ map (\i -> (pack . show $ i, pack . show $ i)) (take 5 ([1,2..] :: [Int]))
{-# INLINE repeatKeyboard #-}
