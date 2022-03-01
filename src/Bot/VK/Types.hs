{-# LANGUAGE FlexibleInstances #-}
module Bot.VK.Types where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable       (toList)
import           Data.Function       ((&))
import           Data.Hashable
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust, isNothing)
import           Data.String         (IsString)
import           Data.Text           (Text, pack)
import           GHC.Exts            (IsString (fromString))
import           GHC.Generics        (Generic)
import           Handlers.Bot
import           Internal.Utils

-- | Type for VK result of a response
data VKUpdateResult = VKUpdateResult {
    newTs   :: Int64,
    updates :: [VKUpdate]
    } deriving (Generic, Show)
instance FromJSON VKUpdateResult where
    parseJSON (Object o) = VKUpdateResult <$> (read <$> o .: "ts") <*> o .: "updates"
    parseJSON _          = mempty
-- | Type for VK user info and instances
data VKUserInfo = VKUserInfo {
      uiId     :: Int64
    , uiPeerId :: Int64
} deriving (Eq, Generic, Ord)
instance Hashable VKUserInfo
instance Show VKUserInfo where
    show VKUserInfo {..} = "user_id: " <> show uiId <> ", peer_id: " <> show uiPeerId

instance FromJSON VKUserInfo where
    parseJSON (Object o) = VKUserInfo <$> (o .: "from_id") <*> (o .: "peer_id")
    parseJSON _          = mempty


-- | Type and instances for gettable vk content
data VKGettable = GText String | GSticker Int64
    deriving (Show)
instance IsString VKGettable where
    fromString s = GText s

instance FromJSON VKGettable where
    parseJSON (Object o) =
        GSticker <$> (o .: "attachments" >>= withArray "attachments array" (mconcat . map findSticker . toList))
        <|>
        GText <$> (o .: "text")
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

-- | Instances for vk keyboard
newtype Button = Button Text deriving Show
instance ToJSON Button where
    toJSON (Button txt) = object ["button" .= txt]
instance ToJSON Keyboard where
    toJSON (Keyboard btns) = object [
        "one_time" .= True,
        "inline"   .= True,
        "buttons"  .= [map (\b -> object
            ["action" .= object [
                "type" .= ("callback" :: String),
                "label" .= b,
                "payload" .= (init . tail . show . encode $ Button b)]]) btns]
        ]

-- | Type for vk sendable message
type VKMessageSend = MessageSend VKGettable VKUserInfo
instance ToJSON VKMessageSend where
    toJSON (MessageSend VKUserInfo{..} (CKeyboard mes kb)) = object [
        if uiId == uiPeerId then "user_id" .= uiId else "chat_id" .= (uiPeerId - 2000000000) ,
        "keyboard" .= kb,
        "message" .= mes
        ]
    toJSON (MessageSend VKUserInfo{..} (CGettable (GText mes))) = object [
        if uiId == uiPeerId then "user_id" .= uiId else "chat_id" .= (uiPeerId - 2000000000) ,
        "message" .= mes
        ]
    toJSON (MessageSend VKUserInfo{..} (CGettable (GSticker id))) = object [
        if uiId == uiPeerId then "user_id" .= uiId else "chat_id" .= (uiPeerId - 2000000000) ,
        "sticker_id" .= id
        ]

-- | Type for vk command
instance FromJSON (Command VKUserInfo) where
    parseJSON obj = parseJSON obj >>= \MessageGet{..} -> do
        case mgContent of
            GText txt -> maybe mempty pure (commandFromString txt mgUserInfo)
            _         -> mempty

-- | Type for vk callback query
instance FromJSON (CallbackQuery VKUserInfo) where
    parseJSON (Object o) = CallbackQuery <$> parseJSON (Object o) <*> o .: "event_id" <*> o .: "payload"
    parseJSON _          = mempty

data CbAnswer = CbAnswer {
    eventId   :: String,
    userId    :: Int64,
    peerId    :: Int64,
    eventData :: Text
}

cbAnswerFromCbQuery :: CallbackQuery VKUserInfo -> Text -> CbAnswer
cbAnswerFromCbQuery CallbackQuery {..} repeatMessage = CbAnswer {
    eventId = cbId,
    userId = cbUserInfo & uiId,
    peerId = cbUserInfo & uiPeerId,
    eventData = repeatMessage
}
instance ToJSON CbAnswer where
    toJSON CbAnswer {..} = object [
            "event_id" .= eventId,
            "user_id"  .= userId,
            "peer_id"  .= peerId,
            "event_data" .= (String . pack . show $ object [
                "type" .= ("show_snackbar" :: String),
                "text" .= eventData
                ])
        ]

-- | Type for vk update query
type VKUpdate = Update VKGettable VKUserInfo
instance FromJSON VKUpdate where
    parseJSON (Object o) = (o .: "type" >>= \t -> guard ((t :: String) == "message_new") >>
        (o .: "object" >>= (.: "message") >>= \m ->
            UCommand <$> parseJSON (Object m)
            <|>
            UMessage <$> parseJSON (Object m)))
        <|> (o .: "type" >>= \t -> guard ((t :: String) == "message_event") >>
            (o .: "object" >>= \e -> UCallbackQuary <$> parseJSON (Object e)))
        <|> pure UnknownUpdate
    parseJSON _ = mempty
