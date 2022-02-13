{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Handlers.Bot
  (Config(..)
  ,Handle(..)
  ,runBot
  ,Message(..)
  ,MessageContent(..)
  ,Command(..)
  ,Update(..)
  ,UpdateContent(..)
  ,UserInfo(..)
  ,Callbackquery(..)
  ,Keyboard(..)
  ,KeyboardMessage(..)
  ) where

import           Control.Monad       (foldM, forever, replicateM_, when)
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

data UpdateContent t s =
    UCCallbackQuary Callbackquery |
    UCMessage (Message t s) |
    UCCommand Command |
    UnknownUpdate
    deriving (Show)

type UserInfo = Int64

data MessageContent t s = MCText t | MCSticker s deriving (Show)
data Message t s = Message {
   mUserInfo       :: UserInfo
 , mMessageContent :: MessageContent t s
 } deriving (Show)

data Callbackquery = Callbackquery {
    cbUserInfo :: UserInfo
  , cbId       :: Int64
  , cbData     :: String
  } deriving (Show)

data Command =
  Help UserInfo |
  Repeat UserInfo |
  Start UserInfo
  deriving (Show)

data Update t s = Update {
    uId     :: Int64
  , uUpdate :: UpdateContent t s
  } deriving (Show)

newtype Keyboard = Keyboard [(Text, Text)]

data KeyboardMessage = KeyboardMessage {
    kmUserInfo :: UserInfo
  , kmMessage  :: Text
  , kmKeyboard :: Keyboard
}

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
data Handle m t s = Handle {
    hConfig           :: Config
  , hLogger           :: L.Handle m
  , hInit             :: (MonadCatch m) => m ()
  , hSleep            :: m ()
  , hGetUpdates       :: (FromJSON (Update t s), MonadCatch m) => Int64 -> m [Update t s]
  , hSendMes          :: (ToJSON (Message t s), IsString t, MonadCatch m) => Message t s -> m ()
  , hSendKeyboard     :: (ToJSON KeyboardMessage, MonadCatch m) => KeyboardMessage -> m ()
  -- | Mostlyor closing opened keyboard
  , hAnsCB            :: (MonadCatch m) => Text -> Callbackquery -> m ()
  -- | Getters and settersor offset and repeat counteror each user
  , hGetOffset        :: m Int64
  , hSetOffset        :: Int64 -> m ()
  , hInsertUserRepeat :: UserInfo -> Int -> m ()
  , hGetUserRepeat    :: UserInfo -> m Int
  }

runBot :: (MonadCatch m,FromJSON (Update t s), IsString t, ToJSON (Message t s), ToJSON KeyboardMessage) =>
  Handle m t s -> m ()
runBot h@Handle {..} = do
  L.info hLogger $ L.JustText "Initializing bot..."
  hInit
  L.info hLogger $ L.JustText "Bot Initialized."
  forever (go h)
  where
    go h = getUpdates h >>= processUpdates h >> hSleep

getUpdates :: (FromJSON (Update t s), MonadCatch m) => Handle m t s -> m [Update t s]
getUpdates Handle {..} = handleEmptyBody $ do
  hGetOffset >>= hGetUpdates
  where
    handleEmptyBody = handle $ \case
      RBodyException EmptyReponseBody -> L.info hLogger (L.JustText "No updates") >> return []
      other                           -> throwM other

processUpdates :: (MonadCatch m, IsString t, ToJSON (Message t s), ToJSON KeyboardMessage) =>
  Handle m t s -> [Update t s] -> m ()
processUpdates h@Handle {..} uls = do
  currentOffset <- hGetOffset
  newOffset <- foldM (\maxOffset (Update uId uc) -> do
    when (uId >= currentOffset) (processUpdateContent h uc)
    if uId >= maxOffset then return uId else return maxOffset) currentOffset uls
  hSetOffset (newOffset + 1)

processUpdateContent :: (MonadCatch m, IsString t, ToJSON (Message t s), ToJSON KeyboardMessage) =>
  Handle m t s -> UpdateContent t s -> m ()
processUpdateContent h@Handle {..} = \case
  UCCallbackQuary cb -> processCallback h cb
  UCCommand c        -> processCommand h c
  UCMessage m        -> processMessage h m
  UnknownUpdate      -> return ()

processCallback :: (MonadCatch m) => Handle m t s -> Callbackquery -> m ()
processCallback Handle {..} cb = do
  oldRepeat <- hGetUserRepeat (cb & cbUserInfo)
  let mNewRepeat = ((readMaybe $ cb & cbData) :: Maybe Int)
  case mNewRepeat of
    Nothing -> L.warning hLogger $ L.JustText ("Bad callback reply from client " <> showUserInfo (cb & cbUserInfo))
    Just newRepeat -> if newRepeat `notElem` [1..5] then
      L.warning hLogger $ L.JustText ("Bad callback reply from client " <> showUserInfo (cb & cbUserInfo))
      else do
        hInsertUserRepeat (cb & cbUserInfo) newRepeat
        hAnsCB (hConfig & cRepeatMes) cb
        L.info hLogger $ L.JustText ("User repeat counter was adjusted and answer was send to " <> showUserInfo (cb & cbUserInfo)
          <> "\n\tOld repeat: " <> (pack . show $ oldRepeat) <> "\n\tNew repeat: " <> (pack . show $ newRepeat))


processCommand :: (IsString t, MonadCatch m, ToJSON KeyboardMessage, ToJSON (Message t s)) => Handle m t s -> Command -> m ()
processCommand Handle {..} = \case
  Start ui  -> do
    hInsertUserRepeat ui $ hConfig & cBaseRepeat
    hSendMes $ Message ui $ MCText (fromString $ unpack (hConfig & cStartMes))
    L.info hLogger $ L.JustText ("New user has been added, info:" <> showUserInfo ui)
  Help ui   -> do
    hSendMes $ Message ui $ MCText (fromString $ unpack (hConfig & cHelpMes))
    L.info hLogger $ L.JustText ("Help message was sent to " <> showUserInfo ui)
  Repeat ui -> do
    hSendKeyboard $ KeyboardMessage ui (hConfig & cRepeatKeyboardMes) repeatKeyboard
    L.info hLogger $ L.JustText ("Keyboard was sent to " <> showUserInfo ui)
  where
    repeatKeyboard = Keyboard $ map (\i -> (pack . show $ i, pack . show $ i)) (take 5 ([1,2..] :: [Int]))

processMessage :: (IsString t, ToJSON (Message t s), MonadCatch m) => Handle m t s -> Message t s -> m ()
processMessage Handle {..} m = do
  userRepeat <- hGetUserRepeat (m & mUserInfo)
  replicateM_ userRepeat (hSendMes m)
  L.info hLogger $ L.JustText ("Message was sent " <> (pack . show $ userRepeat) <> " times to " <> showUserInfo (m & mUserInfo))

showUserInfo :: UserInfo -> Text
showUserInfo ui = "user id: " <> (pack . show $ ui)
