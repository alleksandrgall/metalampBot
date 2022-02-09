{-# LANGUAGE DeriveGeneric #-}

-- | This module will contain generalized response types
-- it is supposed to be imported via qualified
module Bot.Telegram.Internal.ResponseTypesC where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson
import           Data.Char           (toLower)
import           Data.Function       ((&))
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromJust)
import           Data.String         (IsString (fromString))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           GHC.Int             (Int64)

-- | Type for a Telegram response
data Response a = Response
  { responseOk          :: Bool,
    responseDescription :: Maybe Text ,
    responseResult      :: Maybe a
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (Response a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "response"}

-- | Type for a Telegram chat
data Chat = Chat
  {   chatId        :: Int64
    , chatType      :: Text
    , chatTitle     :: Maybe Text
    , chatUsername  :: Maybe Text
    , chatFirstName :: Maybe Text
    , chatLastName  :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "chat"}

-- | Type for a Telegram user
data User = User
  {   userId           :: Int64
    , userIsBot        :: Bool
    , userFirstName    :: Text
    , userLastName     :: Maybe Text
    , userUsername     :: Maybe Text
    , userLanguageCode :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "user"}

-- | Type for a Telegram photo which is an array of PhotoSize (basicaly all photo's thumbnails and previews live with it)

type Photo = [PhotoSize]

newtype PhotoSize = PhotoSize
  { photoSizeFileId :: Text
  }
  deriving (Show, Generic)

instance FromJSON PhotoSize where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "photoSize"}

-- | Type for a Telegram document
newtype Document = Document
  { documentFileId :: Text
  }
  deriving (Show, Generic)

instance FromJSON Document where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "document"}

-- | Type for a Telegram sticker
newtype Sticker = Sticker
  { stickerFileId :: Text
  }
  deriving (Show, Generic)

instance FromJSON Sticker where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "sticker"}

-- | Sum type of all posible mutually exclusive MessageContent message can contain
data MessageContent = MessageContentText Text  | MessageContentPhoto Photo | MessageContentDocument Document | MessageContentSticker Sticker
  deriving (Show, Generic)

instance FromJSON MessageContent where
  parseJSON (Object o) =
    MessageContentText <$> o .: "text" <|> MessageContentPhoto <$> o .: "photo"
      <|> MessageContentDocument <$> o .: "document"
      <|> MessageContentSticker <$> o .: "sticker"
  parseJSON _ = mzero

-- | Type for a Telegram message
data Message = Message
  {   messageId      :: Int
    , messageFrom    :: Maybe User
    , messageDate    :: Int
    , messageChat    :: Chat
    , messageContent :: MessageContent
    , messageCaption :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON (Object o) =
    Message <$> o .: "message_id" <*> o .:? "from" <*> o .: "date" <*> o .: "chat"
      <*> parseJSON (Object o)
      <*> o .:? "caption"
  parseJSON _ = mzero

-- | Type for a Telegram Callback quary returned after a button press
data CallbackQuery = CallbackQuery
  {   callBackQueryId   :: Text
    , callBackQueryFrom :: User
    , callBackQueryData :: Text
  }
  deriving (Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "callBackQuery"}

-- | Sum type for mutually exclusive update content
data UpdateContent = UpdateContentMessage Message | UpdateContentCallbackQuary CallbackQuery
  deriving (Show, Generic)

instance FromJSON UpdateContent where
  parseJSON (Object o) = UpdateContentMessage <$> o .: "message" <|> UpdateContentCallbackQuary <$> o .: "callback_query"
  parseJSON _ = mzero

-- | Type for a Telegram update
data Update = Update
  {   updateId      :: Int
    , updateContent :: UpdateContent
  }
  deriving (Show, Generic)

instance FromJSON Update where
  parseJSON (Object o) = Update <$> o .: "update_id" <*> parseJSON (Object o)
  parseJSON _          = mzero

--
