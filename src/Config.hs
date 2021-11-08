{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Config where


import Control.Exception
import Data.Text
import GHC.Generics
import Conferer --(DefaultConfig (configDef), FromConfig)
import Data.Aeson 
import Conferer.FromConfig (fetchFromConfigWith, FromConfig (fromConfig))

data AppType = Tele | VK deriving (Show)

instance FromConfig AppType where
    fromConfig = fetchFromConfigWith (\s -> case toLower s of 
        "vk" -> Just VK
        "tele" -> Just Tele
        _ -> Nothing)

data AppConfig = AppConfig 
    {
        appConfigToken :: Text,
        appConfigMessenger :: AppType
    } deriving (Generic, Show)

instance FromConfig AppConfig

instance DefaultConfig AppConfig where
    configDef = AppConfig
        {
            appConfigToken = empty , 
            appConfigMessenger = Tele
        }

