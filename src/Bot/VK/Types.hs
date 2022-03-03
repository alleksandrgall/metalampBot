{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot.VK.Types
    (SnackBar (SnackBar),
    VKGettable (GSticker, GText),
    VKMessageSend, VKUpdate,
    VKUpdateResult (newTs, updates),
    VKUserInfo (..))
    where

import           Control.Applicative        (Alternative ((<|>)))
import           Control.Monad              (guard)
import           Data.Aeson                 (FromJSON (parseJSON),
                                             KeyValue ((.=)), ToJSON (toJSON),
                                             Value (Object), encode, object,
                                             withArray, withObject,
                                             withScientific, (.:))
import           Data.Aeson.Types           (Parser)
import qualified Data.ByteString.Lazy.Char8 as CBL (unpack)
import           Data.Hashable              (Hashable)
import           Data.Int                   (Int64)
import           Data.Text                  (Text)
import           GHC.Exts                   (IsList (toList), IsString (..))
import           GHC.Generics               (Generic)
import           Handlers.Bot               (CallbackQuery (CallbackQuery),
                                             Command, Keyboard (..),
                                             MessageGet (..), MessageSend,
                                             Update (..))
import           Internal.Utils             (commandFromString)

-- | Type for VK result of getUpdate
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
    parseJSON (Object o) = VKUserInfo <$> (o .: "from_id" <|> o .: "user_id") <*> (o .: "peer_id")
    parseJSON _          = mempty


-- | Type and instances for gettable vk content
data VKGettable = GText String | GSticker Int64
    deriving (Show)
instance IsString VKGettable where
    fromString s = GText s

instance FromJSON VKGettable where
    parseJSON (Object o) =
        GSticker <$> (o .: "attachments" >>= withArray "attachments array" (mconcat . Prelude.map findSticker . GHC.Exts.toList))
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
instance ToJSON Keyboard where
    toJSON (Keyboard btns) = object [
        "inline"   .= True,
        "buttons"  .= [Prelude.map (\b -> object
            ["action" .= object [
                "type" .= ("callback" :: String),
                "label" .= b,
                "payload" .= b]]) btns]
        ]

-- | Type for vk sendable message
type VKMessageSend = MessageSend VKGettable VKUserInfo

-- | Type for vk command
instance FromJSON (Command VKUserInfo) where
    parseJSON obj = parseJSON obj >>= \MessageGet{..} -> do
        case mgContent of
            GText txt -> maybe mempty pure (commandFromString txt mgUserInfo)
            _         -> mempty


-- | Due to incorrect api behavior payload in callback returns as an int, therefore it should be converted to string first
newtype Payload = Payload { unpayload :: String} deriving (Show)

-- | Instance for vk callback query
instance FromJSON Payload where
    parseJSON (Object o) = Payload <$> (o .: "payload" >>= withScientific "Int payload" (return . show . round))
    parseJSON _ = mempty
instance FromJSON (CallbackQuery VKUserInfo) where
    parseJSON (Object o) = CallbackQuery <$> parseJSON (Object o) <*> o .: "event_id" <*> (unpayload <$> parseJSON (Object o))
    parseJSON _          = mempty

-- | Type for vk update
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

-- | Type for vk event show_snackbar
newtype SnackBar = SnackBar Text

instance ToJSON SnackBar where
    toJSON (SnackBar txt) = object [
        "type" .= ("show_snackbar" :: String),
        "text" .= txt
        ]
