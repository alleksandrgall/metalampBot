{-# LANGUAGE FlexibleInstances #-}
module Bot.Telegram.JSONInstances where

import           Control.Applicative ((<|>))
import           Control.Monad       (foldM, when)
import           Data.Aeson
import           Data.Aeson.Types    (Parser, parseFail)
import           Data.Char           (toLower)
import           Data.Foldable       (Foldable (toList), asum)
import qualified Data.HashMap.Strict as HM
import           Data.Int            (Int64)
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromJust)
import           Data.String         (IsString (fromString))
import           GHC.Generics        (Generic)
import           Handlers.Bot

-- | Type for telegram message and Aeson instances

type TelegramMessage = Message MessageText MessageSticker
instance FromJSON TelegramMessage where
    parseJSON (Object o) = Message <$>
        (o .: "chat" >>= (.: "id"))
        <*> ((MCText <$> parseJSON (Object o)) <|> (MCSticker <$> (o .: "sticker" >>= (parseJSON . Object))))
    parseJSON _ = mempty

instance ToJSON TelegramMessage where
    toJSON (Message ui (MCText MessageText {..})) = object [
          "chat_id" .= ui
        , "text" .= mtText
        , "entities" .= mtEntities
        ]
    toJSON (Message ui (MCSticker MessageSticker {..})) = object [
           "chat_id" .= ui
         , "sticker" .= msId
        ]

-- | Aeson instances for telegram Callbackquery

instance FromJSON Callbackquery where
    parseJSON (Object o) = Callbackquery <$> (o .: "from" >>= (.: "id")) <*> o .: "id" <*> o .: "data"
    parseJSON _ = mempty

-- | Aeson instances for telegram Command
-- If the message contains a command all other content of named message will be ignored
-- If the message contains multiple commands only first one will be processed
instance FromJSON Command where
    parseJSON (Object o) = do
        ui   <- o .: "chat" >>= (.: "id")
        txt  <- (o .: "text" :: Parser String)
        ents <- o .: "entities"
        (start, finish) <- withArray "command start and finish" (findBotCommand . toList) ents
        maybe (parseFail "Unknown command") pure (commandFromString (take finish $ drop start txt) ui)
        where
            findBotCommand :: [Value] -> Parser (Int, Int)
            findBotCommand ar = mconcat $ map
                (withObject "" $ \e -> do
                    t <- (e .: "type" :: Parser String)
                    if t == "bot_command"
                        then (,) <$> e .: "offset" <*> e .: "length"
                        else parseFail "bot command not found") ar
            {-# INLINE findBotCommand #-}
    parseJSON _ = mempty

commandFromString :: String -> UserInfo -> Maybe Command
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Repeat ui
    "/help"   -> Just $ Help ui
    "/start"  -> Just $ Start ui
    _         -> Nothing

-- | Aeson instance for telegram keyboard

instance ToJSON KeyboardMessage where
    toJSON (KeyboardMessage ui mes (Keyboard kb)) = object [
          "chat_id" .= ui
        , "text"    .= mes
        , "reply_markup" .= object ["reply_markup" .= object
            ["inline_keyboard" .=
                [map (\(name, dat) -> object [("text", String name), ("callback_data", String dat)]) kb]]]
     ]


-- | Type and Aeson instances for telegram updates
-- Order of parsers for Update content matters, command parser should be the first one
type TelegramUpdate = Update MessageText MessageSticker
instance FromJSON TelegramUpdate where
    parseJSON (Object o) = Update <$> o .: "update_id" <*>
        ((o .: "message" >>= (\m -> UCCommand <$> parseJSON m <|> UCMessage <$> parseJSON m))
        <|>
        (o .: "callback_query" >>= \cb -> UCCallbackQuary <$> parseJSON cb))
    parseJSON _ = mempty

data MessageText = MessageText {
      mtText     :: String
    , mtEntities :: Maybe [Entity]
} deriving (Show, Generic)

instance IsString MessageText where
    fromString s = MessageText s Nothing

instance FromJSON MessageText where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "mt"}

instance ToJSON MessageText where
    toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "mt"}



newtype MessageSticker = MessageSticker {
    msId :: String
} deriving (Show, Generic)

instance FromJSON MessageSticker where
    parseJSON (Object o) = MessageSticker <$> o .: "file_id"
    parseJSON _          = mempty

data Entity = Entity {
      eType     :: String
    , eOffset   :: Int
    , eLength   :: Int
    , eUrl      :: Maybe String
    , eUser     :: Maybe User
    , eLanguage :: Maybe String
}   deriving (Show, Generic)

instance ToJSON Entity where
    toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "e"}

instance FromJSON Entity where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "e"}

data User = User {
      euId        :: Int64
    , euIsBot     :: Bool
    , euFirstName :: String
} deriving (Show, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "eu"}

instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "eu"}

-- instance FromJSON TelegramMessage where
--     parseJSON (Object o) = Message <$> o .: "from" .: "id" <$>
