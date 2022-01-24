module Config
    -- ( AppConfig (..),
    --   Messenger(..),
    --   fetchConfig)
    where

import           Conferer                       (DefaultConfig (..), FromConfig,
                                                 fetch, mkConfig')
import           Conferer.FromConfig            (FromConfig (fromConfig),
                                                 fetchFromConfigWith)
import           Conferer.Source.CLIArgs        as CLI (fromConfig)
import           Conferer.Source.PropertiesFile as Files (fromFilePath)
import           Conferer.Source.PropertiesFile as Prop (fromConfig)
import           Data.Text                      (Text, empty, toLower)
import           GHC.Generics                   (Generic)

import           Handlers.Logger                (LogLevel (..))
import           Internal.Types                 (Token)

data Messenger = Tele | VK deriving (Show)
instance FromConfig Messenger where
    fromConfig = fetchFromConfigWith (\s -> case toLower s of
        "vk"       -> Just VK
        "tele"     -> Just Tele
        "telegram" -> Just Tele
        _          -> Nothing)

instance FromConfig LogLevel where
    fromConfig = fetchFromConfigWith (\s -> case toLower s of
        "debug"   -> Just Debug
        "warning" -> Just Warning
        "info"    -> Just Info
        "error"   -> Just Error
        _         -> Nothing)

data Repeat = Repeat {
    repeatDefaultNumber :: Int,
    repeatMessage       :: Text
} deriving (Show, Generic)

instance FromConfig Repeat

newtype Help = Help {
    helpMessage :: Text
} deriving (Show, Generic)

instance FromConfig Help

data AppConfig = AppConfig
    {
        appConfigToken     :: Token,
        appConfigMessenger :: Messenger,
        appConfigRepeat    :: Repeat,
        appConfigHelp      :: Help,
        appConfigLogLevel  :: LogLevel
    } deriving (Generic, Show)

instance FromConfig AppConfig

instance DefaultConfig AppConfig where
    configDef = AppConfig
        {
            appConfigToken = "This is a token placeholder" ,
            appConfigMessenger = Tele,
            appConfigRepeat = Repeat {
                repeatDefaultNumber = 1,
                repeatMessage = "This is a default repeat message"},
            appConfigHelp = Help {
                helpMessage = "This is a default help message"},
            appConfigLogLevel = Info
        }

fetchConfig :: IO AppConfig
fetchConfig = fetch =<< mkConfig' [] [CLI.fromConfig, Prop.fromConfig "local", Files.fromFilePath "./config/bot.properties"]
