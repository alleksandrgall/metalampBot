{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RankNTypes        #-}
module Main where
import           Bot.Telegram.Internal.ResponseTypes
import           Config
import           Control.Concurrent.STM              (TVar, newTVarIO)
import           Control.Monad.Catch
import           Data.Aeson                          (KeyValue ((.=)), ToJSON,
                                                      Value (String), encode,
                                                      object, toJSONList)
import           Data.Aeson.Types                    (ToJSON (toJSON))
import qualified Data.ByteString.Lazy                as B
import           Data.Function                       ((&))
import           Data.Int                            (Int64)
import qualified Data.Map                            as M
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T
import           GHC.Generics                        (Generic)
import qualified Handlers.Web                        as WH
import qualified Logger.IO                           as L

import           Control.Monad.ST
import           Data.STRef                          (STRef, newSTRef,
                                                      readSTRef)
import           Data.String                         (IsString (fromString))
import qualified Handlers.Logger                     as LH
import           Internal.Types
import           Network.HTTP.Req                    hiding (Scheme (..))
import           Network.HTTP.Types.Status
import qualified Web.Req                             as W

type NumberOfRepeats = M.Map Int64 Int

data Config = Config {
    cHelpMessage      :: T.Text
  , cDefaultRepeatNum :: Int
  , cToken            :: String
}

data Handle = Handle {
      hConfig                   :: String
    , hLogger                   :: L.Handle IO
    , hOffset                   :: TVar Int
    , hNumberOfRepeats          :: TVar NumberOfRepeats
    , hInit                     :: IO ()
    , hGetUpdates               :: IO [Update]
    , hSendMessage              :: Message -> Int64 -> IO ()
    , hSendRepeatNumberKeyboard :: Int64 -> IO ()
    , hAnswerCallBack           :: CallbackQuery -> Int64 -> IO ()
}


newtype Keyboard = Keyboard [(Text, Text)] deriving (Generic)

instance ToJSON Keyboard where
  toJSON (Keyboard ls) = object ["reply_markup" .= object ["inline_keyboard" .= [map (\(name, dat) -> object [("text", String name), ("callback_data", String dat)]) ls]]]

main = do

  print $ runST $ readSTRef stref
  return (runST $ modifySTRef' stref (+1))
  print $ runST $ readSTRef stref

  return ()
