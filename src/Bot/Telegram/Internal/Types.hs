{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot.Telegram.Internal.Types
    ( TelegramUpdate,
      TelegramMessageSend,
      TelegramMessageGet,
      TelegramGettable(..),
      TelegramUser(..) )
where

import           Control.Applicative ((<|>))
import           Control.Monad       (foldM, when)
import           Data.Aeson          (FromJSON (parseJSON), KeyValue ((.=)),
                                      Options (fieldLabelModifier),
                                      ToJSON (toEncoding, toJSON),
                                      Value (Object, String), camelTo2,
                                      defaultOptions, genericParseJSON,
                                      genericToEncoding, genericToJSON, object,
                                      withArray, withObject, (.:), (.:?))
import           Data.Aeson.Types    (Parser, parseFail)
import           Data.Char           (toLower)
import           Data.Foldable       (Foldable (toList), asum)
import           Data.Function       ((&))
import qualified Data.HashMap.Strict as HM
import           Data.Hashable       (Hashable)
import           Data.Int            (Int64)
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromJust)
import           Data.String         (IsString (fromString))
import           GHC.Generics        (Generic)
import           Handlers.Bot

-- | Type for telegram user (user_id, chat_id)
newtype TelegramUser = TelegramUser (Int64, Int64)
    deriving (Eq, Ord, Hashable, Show)
instance FromJSON TelegramUser where
    parseJSON (Object o) = fmap TelegramUser $ (,) <$> (o .: "from" >>= (.: "id")) <*> (o .: "chat" >>= (.: "id"))
    parseJSON _ = mempty

-- | Type for telegram
data TelegramGettable = GText String (Maybe [Entity]) | GSticker String
    deriving (Show)
instance IsString TelegramGettable where
    fromString s = GText s Nothing

-- | Types for telegram messages and Aeson instances
type TelegramMessageSend = MessageSend TelegramGettable TelegramUser
instance ToJSON TelegramMessageSend where
    toJSON (MessageSend (TelegramUser (uId, chatId)) (CGettable (GText t ents))) = object [
          "chat_id" .= chatId
        , "text" .= t
        , "entities" .= ents
        ]
    toJSON (MessageSend (TelegramUser (uId, chatId)) (CGettable (GSticker sId))) = object [
           "chat_id" .= chatId
         , "sticker" .= sId
        ]
    toJSON (MessageSend (TelegramUser (uId, chatId)) (CKeyboard t (Keyboard kb))) = object [
          "chat_id"  .= chatId
        , "text"     .= t
        , "reply_markup" .= object
            ["inline_keyboard" .=
                [map (\(name, dat) -> object [("text", String name), ("callback_data", String dat)]) kb]]
        ]


type TelegramMessageGet = MessageGet TelegramGettable TelegramUser
instance FromJSON TelegramMessageGet where
    parseJSON (Object o) = MessageGet <$>
        parseJSON (Object o) <*>
         o .: "message_id" <*>
        ((GSticker <$> (o .: "sticker" >>= (.: "file_id")))
        <|>
        (GText <$> (o .: "text") <*> (o .:? "entities")))
    parseJSON _ = mempty

-- | Aeson instances for telegram CallbackQuery
instance FromJSON (CallbackQuery TelegramUser) where
    parseJSON (Object o) = CallbackQuery <$>
        fmap TelegramUser
            ((,) <$> (o .: "from" >>= (.: "id")) <*>
            (o .: "message" >>= (.: "chat") >>= (.: "id"))) <*>
        o .: "id" <*>
        o .: "data"
    parseJSON _ = mempty

-- | Aeson instances for telegram Command
-- If the message contains a command all other content of named message will be ignored
-- If the message contains multiple commands only first one will be processed
instance FromJSON (Command TelegramUser) where
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

commandFromString :: String -> TelegramUser -> Maybe (Command TelegramUser)
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing

-- | Type and Aeson instances for telegram updates
-- Order of parsers for Update content matters, command parser should be the first one
type TelegramUpdate = Update TelegramGettable TelegramUser
instance FromJSON TelegramUpdate where
    parseJSON (Object o) = Update <$> o .: "update_id" <*>
        ((o .: "message" >>= (\m -> UCCommand <$> parseJSON m <|> UCMessage <$> parseJSON m))
        <|>
        (o .: "callback_query" >>= \cb -> UCCallbackQuary <$> parseJSON cb)
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

-- instance FromJSON TelegramMessage where
--     parseJSON (Object o) = Message <$> o .: "from" .: "id" <$>
