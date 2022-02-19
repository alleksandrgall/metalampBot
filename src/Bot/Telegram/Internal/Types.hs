{-# LANGUAGE FlexibleInstances #-}
module Bot.Telegram.Internal.Types
    ( TelegramUpdate,
      TelegramMessageSend,
      TelegramMessageGet,
      TelegramGettable(..))
where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Aeson          (FromJSON (parseJSON), KeyValue ((.=)),
                                      Options (fieldLabelModifier),
                                      ToJSON (toEncoding, toJSON),
                                      Value (Object, String), camelTo2,
                                      defaultOptions, genericParseJSON,
                                      genericToEncoding, genericToJSON, object,
                                      withArray, withObject, (.:), (.:?))
import           Data.Aeson.Types    (Parser)
import           Data.Char           (toLower)
import           Data.Foldable       (Foldable (toList))
import           Data.Hashable       (Hashable)
import           Data.Int            (Int64)
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromJust)
import           Data.String         (IsString (..))
import           GHC.Generics        (Generic)
import           Handlers.Bot        (CallbackQuery (..), Command (..),
                                      CommandType (..), Keyboard (..),
                                      MessageGet (..), MessageSend (..),
                                      SendContent (..), Update (..),
                                      UpdateContent (..), UserInfo (..))

-- | Instances for UserInfo
instance Hashable UserInfo
instance FromJSON UserInfo where
    parseJSON (Object o) = UserInfo <$> (o .: "from" >>= (.: "id")) <*> (o .: "chat" >>= (.: "id"))
    parseJSON _ = mempty

-- | Type for telegram
data TelegramGettable = GText String (Maybe [Entity]) | GSticker String
    deriving (Show)
instance IsString TelegramGettable where
    fromString s = GText s Nothing

-- | Types for telegram messages and Aeson instances
type TelegramMessageSend = MessageSend TelegramGettable
instance ToJSON TelegramMessageSend where
    toJSON (MessageSend UserInfo {..} (CGettable (GText t ents))) = object [
          "chat_id" .= uiChatId
        , "text" .= t
        , "entities" .= ents
        ]
    toJSON (MessageSend UserInfo {..} (CGettable (GSticker sId))) = object [
           "chat_id" .= uiChatId
         , "sticker" .= sId
        ]
    toJSON (MessageSend UserInfo {..} (CKeyboard t (Keyboard kb))) = object [
          "chat_id"  .= uiChatId
        , "text"     .= t
        , "reply_markup" .= object
            ["inline_keyboard" .=
                [map (\(name, dat) -> object [("text", String name), ("callback_data", String dat)]) kb]]
        ]


type TelegramMessageGet = MessageGet TelegramGettable
instance FromJSON TelegramMessageGet where
    parseJSON (Object o) = MessageGet <$>
        parseJSON (Object o) <*>
         o .: "message_id" <*>
        ((GSticker <$> (o .: "sticker" >>= (.: "file_id")))
        <|>
        (GText <$> (o .: "text") <*> (o .:? "entities")))
    parseJSON _ = mempty

-- | Aeson instances for telegram CallbackQuery
instance FromJSON CallbackQuery where
    parseJSON (Object o) = CallbackQuery <$>
        (UserInfo <$> (o .: "from" >>= (.: "id")) <*>
        (o .: "message" >>= (.: "chat") >>= (.: "id"))) <*>
        o .: "id" <*>
        o .: "data"
    parseJSON _ = mempty

-- | Aeson instances for telegram Command
-- If the message contains a command all other content of the fornamed message will be ignored
-- If the message contains multiple commands only first one will be processed
instance FromJSON Command where
    parseJSON (Object o) = do
        ui   <- parseJSON (Object o)
        txt  <- (o .: "text" :: Parser String)
        ents <- o .: "entities"
        (start, finish) <- withArray "command start and finish" (findBotCommand . toList) ents
        maybe mempty pure (commandFromString (take finish $ drop start txt) ui)
        where
            findBotCommand :: [Value] -> Parser (Int, Int)
            findBotCommand ar = mconcat $ map
                (withObject "" $ \e -> do
                    t <- (e .: "type" :: Parser String)
                    if t == "bot_command"
                        then (,) <$> e .: "offset" <*> e .: "length"
                        else mempty) ar
            {-# INLINE findBotCommand #-}
    parseJSON _ = mempty

commandFromString :: String -> UserInfo -> Maybe Command
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing

-- | Type and Aeson instances for telegram updates
-- Order of parsers for Update content matters, command parser should be the first one
type TelegramUpdate = Update TelegramGettable
instance FromJSON TelegramUpdate where
    parseJSON (Object o) = Update <$> o .: "update_id" <*>
        ((o .: "message" >>= (\m -> UCCommand <$> parseJSON m <|> UCMessage <$> parseJSON m))
        <|>
        (o .: "callback_query" >>= (fmap UCCallbackQuary . parseJSON))
        <|>
        pure UnknownUpdate)
    parseJSON _ = mempty

data Entity = Entity {
      eType     :: String
    , eOffset   :: Int
    , eLength   :: Int
    , eUrl      :: Maybe String
    , eUser     :: Maybe EntityUser
    , eLanguage :: Maybe String
}   deriving (Show, Generic)

instance ToJSON Entity where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "e"}

instance FromJSON Entity where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "e"}

data EntityUser = EntityUser {
      euId        :: Int64
    , euIsBot     :: Bool
    , euFirstName :: String
} deriving (Show, Generic)

instance ToJSON EntityUser where
    toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "eu"}

instance FromJSON EntityUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "eu"}
