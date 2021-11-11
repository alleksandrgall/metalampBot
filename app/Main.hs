{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Config
import Conferer
import Data.Function ((&))
import Conferer.Config (addDefault, mkKey)
import Conferer.Source.CLIArgs as CLI
import Conferer.Source.PropertiesFile as Files



main :: IO ()
main = do
    config <- mkConfig' [] [CLI.fromConfig, Files.fromFilePath "./config/bot.properties"]
    (appconf :: AppConfig) <- fetch config
    print appconf
