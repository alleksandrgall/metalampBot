{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Config

main :: IO ()
main = do
    appConfig <- fetchConfig
    print appConfig
