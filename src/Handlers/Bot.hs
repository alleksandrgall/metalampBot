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
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.RWS      (MonadReader)
import           Control.Monad.Reader   (ReaderT (runReaderT))

import           Control.Concurrent.STM (TVar, readTVar)
import           Control.Monad.ST
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Foldable          (traverse_)
import           Data.Int               (Int64)
import           Data.Map               (Map)
import           Data.STRef
import           Data.Text              (Text)
import           Exceptions.Request
import qualified Handlers.Logger        as L
import           Handlers.Web           (Result)
import qualified Handlers.Web           as Web
import           Internal.Types         (Token)
data UpdateContent t s f =
    UCCallbackQuary CallBackquery |
    UCMessage (Message t s f) |
    UCCommand Command

data MessageContent t s f = MCText t | MCSticker s | MCFile f
data Message t s f = Message {
   mChatId         :: Int64
 , mFromId         :: Int64
 , mMessageId      :: Int64
 , mMessageContent :: MessageContent t s f
 }

data CallBackquery = CallBackquery {
    cbChatId :: Int64
  , cbFromId :: Int64
  , cbId     :: Int64
  , cbData   :: String
  }

data Command =
  Help {
    cChatId :: Int64
  , cFromId :: Int64
  } |
  Repeat {
    cChatId :: Int64
  , cFromId :: Int64
  }

data Update t s f = Update {
    uId     :: Int64
  , uUpdate :: UpdateContent t s f
  }

newtype Keyboard = Keyboard [(Text, Text)]

data Config = Config {
    cToken      :: Token
  , cBaseRepeat :: Int
  }

data Handle m t s f = Handle {
    hConfig     :: Config
  , hLogger     :: L.Handle m
  , hInit       :: (MonadCatch m) => m ()
  -- | All of the functions resulting in m () can throw a `Exceptions.Request.RequestException`
  , hGetUpdates :: (FromJSON (Update t s f), MonadCatch m) => m [Update t s f]
  , hReplyMes   :: (ToJSON (Message t s f), MonadCatch m) => Int -> Message t s f -> m ()
  , hAnsCom     :: (ToJSON Keyboard, MonadCatch m) => Keyboard -> Command -> m ()
  , hAnsCB      :: (MonadCatch m) => CallBackquery -> m ()
  , hUserRepeat :: forall thread . STRef thread (Map Int64 Int)
  , hOffset     :: forall thread . STRef thread Int64
  -- | Delay in microsec
  , hDelay      :: Int
  }


processUpdates :: (MonadCatch m, FromJSON (Update t s f), ToJSON (Message t s f), ToJSON Keyboard) =>
  Handle m t s f -> [Update t s f] -> m ()
processUpdates Handle {..} = traverse_ processUpdate
  where
    processUpdate (Update id uc)
      | runST (readSTRef hOffset) > id = return ()
      | otherwise = case uc of

        UCCallbackQuary CallBackquery {..} -> undefined
        UCCommand c                        -> undefined
        UCMessage m                        -> undefined
