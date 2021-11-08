{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Config
import Conferer
import Data.Aeson

main :: IO ()
main = do
    config <- mkConfig "TestConfigApp"
    (appconf :: AppConfig) <- fetch config
    print appconf
