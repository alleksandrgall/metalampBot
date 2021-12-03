{-# LANGUAGE DeriveGeneric #-}

--This module will contain generalized response types
module Bot.ResponseTypes where

import Data.Aeson
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import GHC.Int (Int64)

data Response a = Response
  { responseOk :: Bool,
    responseDescription :: Maybe String,
    responseResult :: Maybe a
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "response" }

data Chat = Chat
  { chatId :: Int64,
    chatType :: String,
    chatTitle :: Maybe String,
    chatUsername :: Maybe String,
    chatFirstName :: Maybe String,
    chatLastName :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "chat" }

data User = User
  { userId :: Int64,
    userIsBot :: Bool,
    userFirstName :: String,
    userLastName :: Maybe String,
    userUsername :: Maybe String,
    userLanguageCode :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "user" }

data PhotoSize = PhotoSize
  { photoSizeFileId :: String
  }
  deriving (Show, Generic)

instance FromJSON PhotoSize where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "photoSize" }

data Document = Document
  { documentFileId :: String
  }
  deriving (Show, Generic)

instance FromJSON Document where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "document" }

data Message = Message
  { messageId :: Int,
    messageFrom :: Maybe User,
    messageDate :: Int,
    messageChat :: Chat,
    messageForwardFrom :: Maybe User,
    messageReplyToMessage :: Maybe Message,
    messageText :: Maybe String,
    messagePhoto :: Maybe [PhotoSize],
    messageDocument :: Maybe Document,
    messageNewChatParticipant :: Maybe User,
    messageLeftChatParticipant :: Maybe User,
    messageNewChatTitle :: Maybe String,
    messageGroupChatCreated :: Maybe Bool
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = 
    (\s -> if s == "id" then "message_id" else s) . camelTo2 '_' . fromJust . stripPrefix "message" }

data Sticker = Sticker
  { stickerFileId :: String
  }
  deriving (Show, Generic)

instance FromJSON Sticker where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "sticker" }

data Update a = Update
  { updateOk :: Bool,
    updateResult :: Maybe a
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Update a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "update" }

