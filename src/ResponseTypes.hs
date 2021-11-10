{-# LANGUAGE DeriveGeneric  #-}
module ResponseTypes where

import Data.Aeson
import GHC.Generics
import Control.Monad 
import Data.Aeson.Types
import Data.Aeson (ToJSON, FromJSON)
import GHC.OldList (stripPrefix)
import Data.Maybe (fromJust)


jsonOpts :: String -> Options
jsonOpts prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . fromJust . stripPrefix prefix }

data TelegramChat = TelegramChat
  { chatId :: Int
  , chatType :: String
  , chatTitle :: Maybe String
  , chatUsername :: Maybe String
  , chatFirstName :: Maybe String
  , chatLastName :: Maybe String
  } deriving (Show, Generic)

instance FromJSON TelegramChat where
    parseJSON = genericParseJSON (jsonOpts "chat")  
