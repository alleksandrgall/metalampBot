{-# LANGUAGE DeriveGeneric  #-}
module ResponseTypes where

import Data.Aeson
import GHC.Generics
import Control.Monad
import Data.Aeson.Types
import GHC.OldList (stripPrefix)
import Data.Maybe (fromJust)


-- Options for modifying feild's names from `prefixCamelStyle` to `camel_style`
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
  
instance ToJSON TelegramChat where
    toJSON = genericToJSON (jsonOpts "chat")  

instance FromJSON TelegramChat where
    parseJSON = genericParseJSON (jsonOpts "chat")

