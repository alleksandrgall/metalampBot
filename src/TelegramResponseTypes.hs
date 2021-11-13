{-# LANGUAGE DeriveGeneric  #-}
module TelegramResponseTypes where

import Data.Aeson
import GHC.Generics
import Control.Monad
import Data.Aeson.Types
import GHC.OldList (stripPrefix)
import Data.Maybe (fromJust)
import GHC.Int (Int64)


-- Options for modifying feild's names from `prefixCamelStyle` to `camel_style`
jsonOpts :: String -> Options
jsonOpts prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix prefix }


data TelegramChat = TelegramChat
  { 
    chatId :: Int64
  , chatType :: String
  , chatTitle :: Maybe String
  , chatUsername :: Maybe String
  , chatFirstName :: Maybe String
  , chatLastName :: Maybe String
  } deriving (Show, Generic)
  
instance FromJSON TelegramChat where
    parseJSON = genericParseJSON (jsonOpts "chat")

data TelegramUser = TelegramUser 
  {
    userId :: Int64
  , userIsBot :: Bool
  , userFirstName :: String
  , userLastName :: Maybe String
  , userUsername :: Maybe String
  , userLanguageCode :: Maybe String
  } deriving (Show, Generic)

instance FromJSON TelegramUser where
  parseJSON = genericParseJSON (jsonOpts "user")

data TelegramMessage = TelegramMessage
  { messageId :: Int
  , messageFrom :: Maybe TelegramUser
  , messageDate :: Int
  , messageChat :: TelegramChat
  , messageForwardFrom :: Maybe TelegramUser
  , messageReplyToMessage :: Maybe TelegramMessage
  , messageText :: Maybe String
  
  , messageNewChatParticipant :: Maybe TelegramUser
  , messageLeftChatParticipant :: Maybe TelegramUser
  , messageNewChatTitle :: Maybe String
  , messageGroupChatCreated :: Maybe Bool
  } deriving (Show, Generic)



