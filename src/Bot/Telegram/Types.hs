{-# LANGUAGE FlexibleInstances #-}
module Bot.Telegram.Types
    ( TelegramUpdate,
      TelegramUserInfo,
      TelegramUpdateWithId(..),
      TelegramMessageSend,
      TelegramMessageGet,
      TelegramGettable(..),
      TelegramResult(..))
where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad       (guard)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Data.Char           (toLower)
import           Data.Foldable       (Foldable (toList))
import           Data.Hashable       (Hashable)
import           Data.Int            (Int64)
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromJust)
import           Data.String         (IsString (..))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Handlers.Bot        (CallbackQuery (..), Command (..),
                                      CommandType (..), MessageGet (..),
                                      MessageSend (..), SendContent (..),
                                      Update (..))

-- | Type for telegram user info and instances
data TelegramUserInfo = TelegramUserInfo {
    uiId     :: Int64,
    uiChatId :: Int64
} deriving (Generic, Eq, Ord)
instance Show TelegramUserInfo where
    show TelegramUserInfo {..} = "user_id: " <> show uiId <> ", chat_id: " <> show uiChatId
instance Hashable TelegramUserInfo

instance FromJSON TelegramUserInfo where
    parseJSON (Object o) = TelegramUserInfo <$> (o .: "from" >>= (.: "id")) <*> (o .: "chat" >>= (.: "id"))
    parseJSON _ = mempty

-- | Type for telegram response
newtype TelegramResult a = TelegramResult a deriving Show
instance (FromJSON a) => FromJSON (TelegramResult a) where
    parseJSON (Object o) = TelegramResult <$> o .: "result"
    parseJSON _          = mempty

-- | Type for telegram gettable content
data TelegramGettable = GText String (Maybe [Entity]) | GSticker String
    deriving (Show, Eq)
instance IsString TelegramGettable where
    fromString s = GText s Nothing

-- | Type and instance for telegram keyboard
data TelegramKeyboard = TelegramKeyboard
keyboardLayout :: [Text]
keyboardLayout = ["1", "2", "3", "4", "5"]

instance ToJSON TelegramKeyboard where
    toJSON _ = object
        ["inline_keyboard" .=
            [map (\name -> object [("text", String name), ("callback_data", String name)]) keyboardLayout]]

-- | Types for telegram messages and Aeson instances
type TelegramMessageSend = MessageSend TelegramGettable TelegramUserInfo
instance ToJSON TelegramMessageSend where
    toJSON (MessageSend TelegramUserInfo {..} (CGettable (GText t ents))) = object [
          "chat_id" .= uiChatId
        , "text" .= t
        , "entities" .= ents
        ]
    toJSON (MessageSend TelegramUserInfo {..} (CGettable (GSticker sId))) = object [
           "chat_id" .= uiChatId
         , "sticker" .= sId
        ]
    toJSON (MessageSend TelegramUserInfo {..} (CKeyboard t )) = object [
          "chat_id"  .= uiChatId
        , "text"     .= t
        , "reply_markup" .= TelegramKeyboard
        ]


type TelegramMessageGet = MessageGet TelegramGettable TelegramUserInfo
instance FromJSON TelegramMessageGet where
    parseJSON (Object o) = MessageGet <$>
        parseJSON (Object o) <*>
        ((GSticker <$> (o .: "sticker" >>= (.: "file_id")))
        <|>
        (GText <$> (o .: "text") <*> (o .:? "entities")))
    parseJSON _ = mempty

-- | Aeson instances for telegram CallbackQuery
instance FromJSON (CallbackQuery TelegramUserInfo) where
    parseJSON (Object o) = CallbackQuery <$>
        (TelegramUserInfo <$> (o .: "from" >>= (.: "id")) <*>
        (o .: "message" >>= (.: "chat") >>= (.: "id"))) <*>
        o .: "id" <*>
        o .: "data"
    parseJSON _ = mempty

-- | Aeson instances for telegram Command
-- If the message contains a command all other content of the fornamed message will be ignored
-- If the message contains multiple commands only first one will be processed

commandFromString :: String -> usInf -> Maybe (Command usInf)
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing

instance FromJSON (Command TelegramUserInfo) where
    parseJSON (Object o) = do
        ui   <- parseJSON (Object o)
        txt  <- (o .: "text" :: Parser String)
        ents <- o .: "entities"
        (start, finish) <- withArray "command start and finish" (findBotCommand . toList) ents
        maybe mempty pure (commandFromString (take finish $ drop start txt) ui)
        where
            findBotCommand :: [Value] -> Parser (Int, Int)
            findBotCommand = mconcat . map
                (withObject "" $ \e -> do
                    t <- (e .: "type" :: Parser String)
                    guard (t == "bot_command")
                    (,) <$> e .: "offset" <*> e .: "length")
            {-# INLINE findBotCommand #-}
    parseJSON _ = mempty

-- | Type and Aeson instances for telegram updates
-- Order of parsers for Update constructors matter, command parser should be the first one
type TelegramUpdate = Update TelegramGettable TelegramUserInfo
data TelegramUpdateWithId = TelegramUpdateWithId {
      uId   :: Int64
    , uCont :: TelegramUpdate
} deriving (Show, Eq)

instance FromJSON TelegramUpdate where
    parseJSON (Object o) =
        (o .: "message" >>=
            (\m -> UCommand <$> parseJSON m <|> UMessage <$> parseJSON m))
        <|>
        (o .: "callback_query" >>= (fmap UCallbackQuary . parseJSON))
        <|>
        pure UnknownUpdate
    parseJSON _ = mempty

instance FromJSON TelegramUpdateWithId where
    parseJSON (Object o) = TelegramUpdateWithId <$> o .: "update_id" <*> parseJSON (Object o)
    parseJSON _ = mempty

data Entity = Entity {
      eType     :: String
    , eOffset   :: Int
    , eLength   :: Int
    , eUrl      :: Maybe String
    , eUser     :: Maybe EntityUser
    , eLanguage :: Maybe String
}   deriving (Show, Generic, Eq)

instance ToJSON Entity where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "e"}

instance FromJSON Entity where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "e"}

data EntityUser = EntityUser {
      euId        :: Int64
    , euIsBot     :: Bool
    , euFirstName :: String
} deriving (Show, Generic, Eq)

instance ToJSON EntityUser where
    toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "eu"}

instance FromJSON EntityUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix "eu"}
