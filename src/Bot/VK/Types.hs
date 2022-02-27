{-# LANGUAGE FlexibleInstances #-}
module Bot.VK.Types where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable       (toList)
import           Data.Hashable
import           Data.Int            (Int64)
import           Data.String         (IsString)
import           Data.Text           (Text, pack)
import           GHC.Exts            (IsString (fromString))
import           GHC.Generics        (Generic)
import           Handlers.Bot
import           Internal.Utils

-- | Type for VK user info and instances
data VKUserInfo = VKUserInfo {
      uiId     :: Int64
    , uiChatId :: Maybe Int64
} deriving (Eq, Generic, Ord)
instance Show VKUserInfo where
    show VKUserInfo {..} = "user_id: " <> show uiId <> maybe "" (\i -> "chat_id: " <> show i) uiChatId

-- | Type for VK result of a response
newtype VKResult a = VKResult a deriving Show
instance (FromJSON a) => FromJSON (VKResult a) where
    parseJSON (Object o) = VKResult <$> o .: "response"
    parseJSON _          = mempty

instance FromJSON VKUserInfo where
    parseJSON (Object o) = VKUserInfo <$> (o .: "from_id") <*> fmap (fmap (\x -> x - 2000000000)) (o .:? "peer_id")
    parseJSON _          = mempty

--(o .: "type" >>= \t -> guard (t == "message_new")) >>

-- | Type and instances for gettable vk content
data VKGettable = GText String | GSticker Int64
    deriving (Show)
instance IsString VKGettable where
    fromString s = GText s

instance FromJSON VKGettable where
    parseJSON (Object o) =
     GText <$> (o .: "text")
     <|>
     GSticker <$> (o .: "attachments" >>= withArray "attachments array" ((mconcat . map findSticker) . toList))
     where
        findSticker = withObject "attachement" $ \at -> do
            t <- (at .: "type" :: Parser String)
            guard (t == "sticker")
            at .: "sticker" >>= (.: "sticker_id")
    parseJSON _ = mempty

-- | Type for vk gettable message
type VKMessageGet = MessageGet VKGettable VKUserInfo

instance FromJSON VKMessageGet where
    parseJSON (Object o) = MessageGet <$> parseJSON (Object o) <*> parseJSON (Object o)
    parseJSON _          = mempty

-- | Type for vk sendable message
type VKMessageSend = MessageSend VKGettable VKUserInfo

instance ToJSON VKMessageSend where
    toJSON MessageSend {..} = undefined

-- | Type for vk command
instance FromJSON (Command VKUserInfo) where
    parseJSON obj = parseJSON obj >>= \MessageGet{..} -> do
        case mgContent of
            GText txt -> maybe mempty pure (commandFromString txt mgUserInfo)
            _         -> mempty

-- | Type for vk callback query
instance FromJSON (CallbackQuery VKUserInfo) where
    parseJSON (Object o) = undefined
    parseJSON _          = mempty

-- | Type for vk update query
type VKUpdate = Update VKGettable


