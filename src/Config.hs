{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Config where



import Data.Text ( toLower, empty, Text )
import GHC.Generics ( Generic )
import Conferer ( DefaultConfig(..), FromConfig ) --(DefaultConfig (configDef), FromConfig)
import Data.Aeson 
import Conferer.FromConfig (fetchFromConfigWith, FromConfig (fromConfig))

data Messenger = Tele | VK deriving (Show)

instance FromConfig Messenger where
    fromConfig = fetchFromConfigWith (\s -> case toLower s of 
        "vk" -> Just VK
        "tele" -> Just Tele
        "telegram" -> Just Tele
        _ -> Nothing)

data Repeat = Repeat {
    repeatDefaultNumber :: Int,
    repeatMessage :: Text
} deriving (Show, Generic)

instance FromConfig Repeat

data Help = Help {
    helpMessage :: Text
} deriving (Show, Generic)

instance FromConfig Help

data AppConfig = AppConfig 
    {
        appConfigToken :: Text,
        appConfigMessenger :: Messenger,
        appConfigRepeat :: Repeat,
        appConfigHelp :: Help

    } deriving (Generic, Show)

instance FromConfig AppConfig

instance DefaultConfig AppConfig where
    configDef = AppConfig
        {
            appConfigToken = "This is token placeholder" , 
            appConfigMessenger = Tele,
            appConfigRepeat = Repeat {
                repeatDefaultNumber = 1, 
                repeatMessage = "This is default repeat message"},
            appConfigHelp = Help {
                helpMessage = "This is default help message"}
        }

