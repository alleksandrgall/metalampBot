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

data Messenger = Tele | VK deriving (Show, Eq)
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

newtype GroupId = GroupId Int deriving (Show, Generic, Eq)
instance FromConfig GroupId

data Repeat = Repeat {
    repeatDefaultNumber :: Int,
    repeatKeyboardMes   :: Text,
    repeatMessage       :: Text
} deriving (Show, Generic)

instance FromConfig Repeat

newtype Help = Help {
    helpMessage :: Text
} deriving (Show, Generic)

instance FromConfig Help

newtype Start = Start {
    startMessage :: Text
} deriving (Show, Generic)
instance FromConfig Start

data Logger = Logger {
    loggerLogLevel   :: LogLevel,
    loggerOutputFile :: Maybe FilePath,
    loggerToConsole  :: Bool
} deriving (Generic, Show)

instance FromConfig Logger

data AppConfig = AppConfig
    {
        appConfigToken     :: Token,
        appConfigMessenger :: Messenger,
        appConfigStart     :: Start,
        appConfigRepeat    :: Repeat,
        appConfigHelp      :: Help,
        appConfigLogger    :: Logger,
        appConfigGroupId   :: GroupId
    } deriving (Generic, Show)

instance FromConfig AppConfig

instance DefaultConfig AppConfig where
    configDef = AppConfig
        {
            appConfigToken = "" ,
            appConfigMessenger = Tele,
            appConfigStart = Start {
                startMessage = "This is a start message"
            },
            appConfigRepeat = Repeat {
                repeatDefaultNumber = 1,
                repeatKeyboardMes = "This is a default repeat message",
                repeatMessage = "This is a default message for repeat keyboard"},
            appConfigHelp = Help {
                helpMessage = "This is a default help message"},
            appConfigLogger = Logger {
                loggerLogLevel = Info,
                loggerOutputFile = Nothing,
                loggerToConsole  = True
            },
            appConfigGroupId = GroupId (-1)
        }

fetchConfig :: IO AppConfig
fetchConfig = fetch =<< mkConfig' [] [CLI.fromConfig, Prop.fromConfig "local", Files.fromFilePath "./config/bot.properties"]
