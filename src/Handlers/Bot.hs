{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Handlers.Bot
  (MessageContent(..)
  ,Command(..)
  ,UpdateContent(..)
  ,Update(..)
  ,Config(..)
  ,Handle(..)
  ) where

--import           Control.Concurrent.STM (TVar)
--import           Control.Concurrent.STM (TVar)
--import           Control.Concurrent.STM (TVar)
--import           Control.Concurrent.STM (TVar)
import           Control.Monad        (foldM, when)
import           Control.Monad.Catch  (MonadCatch, MonadThrow (throwM), handle)
import           Control.Monad.RWS    (MonadReader)
import           Control.Monad.Reader (ReaderT (runReaderT))
import           Control.Monad.ST     (runST)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Function        ((&))
import           Data.Int             (Int64)
import           Data.Map             (Map, insert)
import           Data.STRef           (STRef, modifySTRef', readSTRef,
                                       writeSTRef)
import           Data.String          (IsString (..))
import           Data.Text            (Text, unpack)
import           Exceptions.Request
import qualified Handlers.Logger      as L
import           Handlers.Web         (Result)
import qualified Handlers.Web         as Web
import           Internal.Types       (Token)
data UpdateContent t s f =
    UCCallbackQuary CallBackquery |
    UCMessage (Message t s f) |
    UCCommand Command

data UserInfo = UserInfo {
    fromId :: Int64
  , chatId :: Int64
} deriving (Eq)

instance Ord UserInfo where
  ui1 <= ui2 = (ui1 & fromId) <= (ui2 & fromId)

data MessageContent t s f = MCText t | MCSticker s | MCFile f
data Message t s f = Message {
   mUserInfo       :: UserInfo
 , mMessageContent :: MessageContent t s f
 }

data CallBackquery = CallBackquery {
    cbUserInfo :: UserInfo
  , cbId       :: Int64
  , cbData     :: String
  }

data Command =
  Help UserInfo |
  Repeat UserInfo |
  Start UserInfo

data Update t s f = Update {
    uId     :: Int64
  , uUpdate :: UpdateContent t s f
  }

newtype Keyboard = Keyboard [(Text, Text)]

data Config = Config {
    cToken      :: Token
  , cBaseRepeat :: Int
  , cStartMes   :: Text
  , cHelpMes    :: Text
  , cRepeatMes  :: Text
  }

data Handle m t s f = Handle {
    hConfig     :: Config
  , hLogger     :: L.Handle m
  -- | All of the functions resulting in `m a` can throw a `Exceptions.Request.RequestException`
  , hInit       :: (MonadCatch m) => m ()
  , hGetUpdates :: (FromJSON (Update t s f), MonadCatch m) => Int64 -> m [Update t s f]
  , hSendMes    :: (ToJSON (Message t s f), IsString t, MonadCatch m) => Message t s f -> m ()
  , hAnsCB      :: (MonadCatch m) => CallBackquery -> m ()
  , hUserRepeat :: forall thread . STRef thread (Map UserInfo Int)
  , hOffset     :: forall thread . STRef thread Int64
  -- | Delay in microsec
  , hDelay      :: Int
  }

getUpdates :: (FromJSON (Update t s f), MonadCatch m) => Handle m t s f -> m [Update t s f]
getUpdates Handle {..} = handleEmptyBody $ do
  hGetUpdates (runST $ readSTRef hOffset)
  where
    handleEmptyBody = handle $ \case
      RBodyException (EmptyReponseBody t) -> L.info hLogger (L.JustText "No updates") >> return []
      other                               -> throwM other

processUpdates :: (MonadCatch m, FromJSON (Update t s f), IsString t, ToJSON (Message t s f), ToJSON Keyboard) =>
  Handle m t s f -> [Update t s f] -> m ()
processUpdates Handle {..} uls = do
  let currentOffset = runST $ readSTRef hOffset
  newOffset <- foldM (\maxOffset (Update id uc) -> do
    when (id >= currentOffset) (processUpdateContent uc)
    if id >= maxOffset then return id else return maxOffset) currentOffset uls
  return (runST $ writeSTRef hOffset newOffset)
  where
    processUpdateContent = \case
          UCCallbackQuary CallBackquery {..} -> undefined
          UCCommand c                        -> case c of
            Start ui  -> do
              return (runST $ modifySTRef' hUserRepeat $ \m -> insert ui (hConfig & cBaseRepeat) m)
              hSendMes (Message ui $ MCText (fromString (unpack (hConfig & cStartMes))))
            Help UserInfo {..}   -> undefined
            Repeat UserInfo {..} -> undefined
          UCMessage m                        -> undefined
