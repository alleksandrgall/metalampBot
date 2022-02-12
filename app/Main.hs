{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RankNTypes        #-}
module Main where
import           Bot.Telegram.JSONInstances
import           Data.Aeson
import           Data.Aeson.Types           (ToJSON (toJSON))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Handlers.Bot

newtype Keyboard = Keyboard [(Text, Text)] deriving (Generic)

instance ToJSON Keyboard where
  toJSON (Keyboard ls) =
    object ["reply_markup" .= object
      ["inline_keyboard" .=
        [map (\(name, dat) -> object [("text", String name), ("callback_data", String dat)]) ls]]]



main = do
  mt <- (eitherDecodeFileStrict' "temp/TGMessageRepeat.json" :: IO (Either String Command))
  print mt
  return ()
