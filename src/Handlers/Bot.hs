{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Handlers.Bot
  (Config(..)
  ,Handle(..)
  ,runBot
  ,MessageGet(..)
  ,MessageSend(..)
  ,UserMesContent(..)
  ,SendMesContent(..)
  ,Command(..)
  ,CommandType(..)
  ,Update(..)
  ,UpdateContent(..)
  ,UserInfo(..)
  ,Callbackquery(..)
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

data UserMesContent t s = CText t | CSticker s deriving (Show)

data MessageGet t s = MessageGet {
    mgUserInfo  :: UserInfo
  , mgMessageId :: Int
  , mgContent   :: UserMesContent t s
} deriving Show


data MessageSend t s = MessageSend {
    msUserInfo :: UserInfo
  , msContent  :: SendMesContent t s
} deriving Show

data SendMesContent t s = ToUser (UserMesContent t s) | CKeyboard t Keyboard deriving Show

data UpdateContent t s =
    UCCallbackQuary Callbackquery |
    UCMessage (MessageGet t s) |
    UCCommand Command |
    UnknownUpdate
    deriving (Show)

data Callbackquery = Callbackquery {
    cbUserInfo :: UserInfo
  , cbId       :: String
  , cbData     :: String
  } deriving (Show)

data Command = Command {
    cUserInfo    :: UserInfo
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

data Update t s = Update {
    uId     :: Int64
  , uUpdate :: UpdateContent t s
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
data Handle t s m = Handle {
    hConfig           :: Config
  , hLogger           :: L.Handle m
  , hInit             :: (MonadCatch m) => m ()
  , hSleep            :: m ()
  , hGetUpdates       :: (FromJSON (Update t s), MonadCatch m) => Int64 -> m [Update t s]
  , hSendMes          :: (ToJSON (MessageSend t s), IsString t, MonadCatch m) => MessageSend t s -> m ()
  -- | Notify the server and the user that button press was processed
  , hAnswerCallback   :: (MonadCatch m) => Text -> Callbackquery -> m ()
  -- | Getters and settersor offset and repeat counteror each user
  , hGetOffset        :: m Int64
  , hSetOffset        :: Int64 -> m ()
  , hInsertUserRepeat :: UserInfo -> Int -> m ()
  , hGetUserRepeat    :: UserInfo -> m (Maybe Int)
  }

runBot :: (MonadCatch m, FromJSON (Update t s), IsString t, ToJSON (MessageSend t s)) =>
  Handle t s m -> m ()
runBot h@Handle {..} = do
  L.info hLogger $ L.JustText "Initializing bot..."
  hInit
  L.info hLogger $ L.JustText "Bot Initialized."
  forever (go h)
  where
    go h = getUpdates h >>= (\us -> unless (null us) (processUpdates h us)) >> hSleep

getUpdates :: (FromJSON (Update t s), MonadCatch m) => Handle t s m -> m [Update t s]
getUpdates Handle {..} = handleEmptyBody $ do
  offset <- hGetOffset
  L.info hLogger $ L.JustText ("Getting updates with offset: " <> (pack . show $ offset) <> "...")
  updates <- hGetUpdates offset
  L.info hLogger $ L.JustText "Got updates."
  return updates
  where
    handleEmptyBody = handle $ \case
      RBodyException EmptyReponseBody -> L.info hLogger (L.JustText "No updates") >> return []
      other                           -> throwM other

processUpdates :: (MonadCatch m, IsString t, ToJSON (MessageSend t s)) =>
  Handle t s m -> [Update t s] -> m ()
processUpdates h@Handle {..} uls = do
  L.info hLogger $ L.JustText "Processing updates..."
  currentOffset <- hGetOffset
  newOffset <- foldM (\maxOffset (Update uId uc) -> do
    when (uId >= currentOffset) (processUpdateContent h uc)
    if uId >= maxOffset then return uId else return maxOffset) currentOffset uls
  L.info hLogger $ L.JustText "Updates processed."
  hSetOffset (newOffset + 1)

processUpdateContent :: (MonadCatch m, IsString t, ToJSON (MessageSend t s)) =>
  Handle t s m -> UpdateContent t s -> m ()
processUpdateContent h@Handle {..} = \case
  UCCallbackQuary cb -> processCallback h cb
  UCCommand c        -> processCommand h c
  UCMessage m        -> processMessage h m
  UnknownUpdate      -> return ()

processCallback :: (MonadCatch m) => Handle t s m -> Callbackquery -> m ()
processCallback Handle {..} cb = do
  L.info hLogger $ L.JustText ("Processing callback from user: " <> showUserInfo (cb & cbUserInfo) <> "...")
  oldRepeat <- hGetUserRepeat (cb & cbUserInfo)
  let mNewRepeat = ((readMaybe $ cb & cbData) :: Maybe Int)
  case mNewRepeat of
    Nothing -> L.warning hLogger $ L.JustText ("Bad callback reply from client " <> showUserInfo (cb & cbUserInfo))
    Just newRepeat -> if newRepeat `notElem` [1..5] then
      L.warning hLogger $ L.JustText ("Bad callback reply from client " <> showUserInfo (cb & cbUserInfo))
      else do
        hInsertUserRepeat (cb & cbUserInfo) newRepeat
        hAnswerCallback (hConfig & cRepeatMes) cb
        L.info hLogger $ L.JustText ("User repeat counter was adjusted and answer was send to " <> showUserInfo (cb & cbUserInfo)
          <> "\n\tOld repeat: " <> (pack . show $ oldRepeat) <> "\n\tNew repeat: " <> (pack . show $ newRepeat))


processCommand :: (IsString t, MonadCatch m, ToJSON (MessageSend t s)) => Handle t s m -> Command -> m ()
processCommand Handle {..} Command {..} = do
  L.info hLogger $ L.JustText ("Processing command " <> (pack . show $ cCommandType) <> " from user: " <> (pack . show $ cUserInfo) <> "...")
  case cCommandType of
    Start -> do
      hSendMes $ MessageSend cUserInfo (ToUser $ CText (fromString $ unpack (hConfig & cStartMes)))
      L.info hLogger $ L.JustText ("New user has been added, info:" <> showUserInfo cUserInfo)
    Help -> do
      hSendMes $ MessageSend cUserInfo (ToUser $ CText (fromString $ unpack (hConfig & cHelpMes)))
      L.info hLogger $ L.JustText ("Help message was sent to " <> showUserInfo cUserInfo)
    Repeat -> do
      hSendMes $ MessageSend cUserInfo $ CKeyboard (fromString $ unpack (hConfig & cRepeatKeyboardMes)) repeatKeyboard
      L.info hLogger $ L.JustText ("Keyboard was sent to " <> showUserInfo cUserInfo)

processMessage :: (IsString t, ToJSON (MessageSend t s), MonadCatch m) => Handle t s m -> MessageGet t s -> m ()
processMessage Handle {..} MessageGet {..} = do
  L.info hLogger $ L.JustText ("Processing message from user: " <> (pack . show $ mgUserInfo))
  userRepeat <- handleNoUser mgUserInfo =<< hGetUserRepeat mgUserInfo
  replicateM_ userRepeat (hSendMes $ MessageSend mgUserInfo (ToUser mgContent))
  L.info hLogger $ L.JustText ("Message was sent " <> (pack . show $ userRepeat) <> " times to " <> showUserInfo mgUserInfo)
  where
    handleNoUser ui = maybe (hInsertUserRepeat ui (hConfig & cBaseRepeat) >> return (hConfig & cBaseRepeat)) return

repeatKeyboard :: Keyboard
repeatKeyboard = Keyboard $ map (\i -> (pack . show $ i, pack . show $ i)) (take 5 ([1,2..] :: [Int]))
{-# INLINE repeatKeyboard #-}

showUserInfo :: UserInfo -> Text
showUserInfo ui = "user id: " <> (pack . show $ ui)
