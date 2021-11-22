{-# LANGUAGE DeriveGeneric  #-}
--This module will contain generalized response types
module Bot.ResponseTypes where

import Data.Aeson
import GHC.Generics ( Generic )
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import GHC.Int (Int64)
import Data.Function ((&))

data Response a = Response 
  { responseOk :: Bool
  , responseDescription :: Maybe String
  , responseResult :: Maybe a
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = genericParseJSON (labelToSnakeCase "response")

data Chat = Chat
  { chatId :: Int64
  , chatType :: String
  , chatTitle :: Maybe String
  , chatUsername :: Maybe String
  , chatFirstName :: Maybe String
  , chatLastName :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Chat where
    parseJSON = genericParseJSON (labelToSnakeCase "chat")

data User = User 
  { userId :: Int64
  , userIsBot :: Bool
  , userFirstName :: String
  , userLastName :: Maybe String
  , userUsername :: Maybe String
  , userLanguageCode :: Maybe String
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (labelToSnakeCase "user")

data PhotoSize = PhotoSize 
  { photoSizeFileId :: String
  } deriving (Show, Generic)

instance FromJSON PhotoSize where
  parseJSON = genericParseJSON (labelToSnakeCase "photoSize")

data Document = Document
  { documentFileId :: String
  } deriving (Show, Generic)

instance FromJSON Document where
  parseJSON = genericParseJSON (labelToSnakeCase "document")

data Message = Message
  { messageId :: Int
  , messageFrom :: Maybe User
  , messageDate :: Int
  , messageChat :: Chat
  , messageForwardFrom :: Maybe  User
  , messageReplyToMessage :: Maybe Message
  , messageText :: Maybe String
  , messagePhoto :: Maybe [PhotoSize]
  , messageDocument :: Maybe Document
  , messageNewChatParticipant :: Maybe User
  , messageLeftChatParticipant :: Maybe User
  , messageNewChatTitle :: Maybe String
  , messageGroupChatCreated :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON (labelToSnakeCase "message" & addNameToId "message")

data Sticker = Sticker 
  { stickerFileId :: String
  } deriving (Show, Generic)

data Update a = Update {
  updateOk :: Bool,
  updateResult :: Maybe a
} deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Update a) where
  parseJSON = genericParseJSON (labelToSnakeCase "update")

instance FromJSON Sticker where
  parseJSON = genericParseJSON (labelToSnakeCase "sticker")

-- Options for modifying feild's names from `prefixCamelStyle` to `camel_style`
labelToSnakeCase :: String -> Options
labelToSnakeCase prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix prefix }

addNameToId :: String -> Options -> Options
addNameToId name opts = opts { fieldLabelModifier = \label -> if label == "id" then name ++ "_id" else label }  