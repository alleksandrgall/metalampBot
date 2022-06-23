{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.Bot where

import Control.Monad (foldM, replicateM_)
import Data.Bifunctor (first)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import qualified Handlers.Logger as L
import Internal.ShowText (showText)
import Text.Read (readMaybe)

data MessageGet gettable usInf = MessageGet
  { mgUserInfo :: usInf,
    mgContent :: gettable
  }
  deriving (Show, Eq)

data SendContent gettable = CGettable gettable | CKeyboard Text
  deriving (Show, Eq)

instance (IsString gettable) => IsString (SendContent gettable) where
  fromString s = CGettable . fromString $ s

data MessageSend gettable usInf = MessageSend
  { msUserInfo :: usInf,
    msContent :: SendContent gettable
  }
  deriving (Show, Eq)

data CallbackQuery usInf = CallbackQuery
  { cbUserInfo :: usInf,
    cbId :: Text,
    cbData :: Text
  }
  deriving (Show, Eq)

data Command usInf = Command
  { cUserInfo :: usInf,
    cCommandType :: CommandType
  }
  deriving (Show, Eq)

data CommandType
  = Help
  | Repeat
  | Start
  deriving (Eq)

instance Show CommandType where
  show Help = "/help"
  show Repeat = "/repeat"
  show Start = "/start"

data Update gettable usInf
  = UCallbackQuary (CallbackQuery usInf)
  | UMessage (MessageGet gettable usInf)
  | UCommand (Command usInf)
  | UnknownUpdate
  deriving (Show, Eq)

data Config = Config
  { cBaseRepeat :: Int,
    cStartMes :: Text,
    cHelpMes :: Text,
    cRepeatMes :: Text,
    cRepeatKeyboardMes :: Text
  }

data Handle gettable usInf m = (Monad m, Eq usInf, Hashable usInf, Show usInf) =>
  Handle
  { hConfig :: Config,
    hLogger :: L.Handle m,
    hInit :: m Int64,
    hGetUpdates :: Int64 -> m (Maybe Int64, [Update gettable usInf]),
    hSendMes :: (IsString gettable) => MessageSend gettable usInf -> m (),
    hAnswerCallback :: Text -> CallbackQuery usInf -> m ()
  }

runBot ::
  (IsString gettable, Show usInf) =>
  Handle gettable usInf m ->
  m ()
runBot h@Handle {..} = do
  L.info hLogger "Initializing bot..."
  startingOffset <- hInit
  L.info hLogger "Bot has been initialized."
  botLoop h startingOffset (HM.fromList [])

botLoop :: (IsString gettable) => Handle gettable usInf m -> Int64 -> HM.HashMap usInf Int -> m ()
botLoop h@Handle {..} offset userRepeats = do
  (maybeOffset, uls) <- hGetUpdates offset
  case maybeOffset of
    Nothing -> botLoop h offset userRepeats
    Just newOffset -> do
      newUserRepeats <- processUpdates h userRepeats uls
      botLoop h newOffset newUserRepeats

processUpdates ::
  (IsString gettable) =>
  Handle gettable usInf m ->
  HM.HashMap usInf Int ->
  [Update gettable usInf] ->
  m (HM.HashMap usInf Int)
processUpdates h@Handle {..} userRepeats uls = do
  L.info hLogger "Processing updates..."
  newUserRepeats <- foldM processUpdateContent userRepeats uls
  L.info hLogger "Updates were processed."
  return newUserRepeats
  where
    processUpdateContent accUserRepeats upd = case upd of
      UCallbackQuary cb -> processCallback h cb accUserRepeats
      UCommand c -> processCommand h c >> return accUserRepeats
      UMessage m -> processMessage h m accUserRepeats >> return accUserRepeats
      UnknownUpdate -> L.info hLogger "Got an unknown update." >> return accUserRepeats

newtype RepeatNum = RepeatNum Int deriving (Enum, Eq, Ord, Num, Real, Integral)

instance Show RepeatNum where
  show (RepeatNum x) = show x

instance Read RepeatNum where
  readsPrec n s =
    let readInt = readsPrec n s :: [(Int, String)]
     in map (first RepeatNum) $ filter ((`elem` [1 .. 5]) . fst) readInt

processCallback :: Handle gettable usInf m -> CallbackQuery usInf -> HM.HashMap usInf Int -> m (HM.HashMap usInf Int)
processCallback Handle {..} cb@CallbackQuery {..} userRepeats = do
  L.info hLogger ("Processing callback from the " <> showText cbUserInfo <> "...")
  let maybeNewRepeat = readMaybe . unpack $ cbData :: Maybe RepeatNum
  case maybeNewRepeat of
    Nothing ->
      L.warning hLogger ("Bad callback data from the " <> showText cbUserInfo <> ", data: " <> cbData)
        >> return userRepeats
    Just newRepeatNum -> do
      hAnswerCallback (hConfig & cRepeatMes) cb
      L.info
        hLogger
        ( "Repeat number of the " <> showText cbUserInfo <> " was adjusted and answer was send to the user\n\t"
            <> "New number: "
            <> showText newRepeatNum
        )
      return $ HM.insert cbUserInfo (fromIntegral newRepeatNum) userRepeats

processCommand :: (IsString gettable) => Handle gettable usInf m -> Command usInf -> m ()
processCommand Handle {..} Command {..} = do
  L.info hLogger ("Processing command " <> showText cCommandType <> " from the " <> showText cUserInfo <> "...")
  case cCommandType of
    Start -> do
      hSendMes $ MessageSend cUserInfo (fromString . unpack $ hConfig & cStartMes)
      L.info hLogger ("New user has been added, info:" <> showText cUserInfo)
    Help -> do
      hSendMes $ MessageSend cUserInfo (fromString . unpack $ hConfig & cHelpMes)
      L.info hLogger ("Help message was sent to the " <> showText cUserInfo)
    Repeat -> do
      hSendMes $ MessageSend cUserInfo $ CKeyboard (hConfig & cRepeatKeyboardMes)
      L.info hLogger ("Keyboard was sent to the " <> showText cUserInfo)

processMessage :: (IsString gettable) => Handle gettable usInf m -> MessageGet gettable usInf -> HM.HashMap usInf Int -> m ()
processMessage Handle {..} MessageGet {..} userRepeats = do
  L.info hLogger $ "Processing message from the " <> showText mgUserInfo
  userRepeat <- maybe (return (hConfig & cBaseRepeat)) return $ HM.lookup mgUserInfo userRepeats
  replicateM_ userRepeat $ hSendMes (MessageSend mgUserInfo (CGettable mgContent))
  L.info hLogger $ "Message was sent " <> showText userRepeat <> " times to the " <> showText mgUserInfo
