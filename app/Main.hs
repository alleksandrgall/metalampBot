{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Main where

import Config
import Bot.ResponseTypes
import Control.Concurrent.STM ( newTVarIO, modifyTVar', STM, atomically, readTVarIO )
import Control.Monad.Reader ( Monad(return), Functor(fmap) )
import qualified Data.Text as T
import Prelude hiding (log)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Except (runExcept, runExceptT)
import Network.HTTP.Req 
import Data.Aeson.Types (parseEither)
import Data.Function ((&))
import Control.Exception (throwIO)
import Data.Either (isLeft)
import Data.Kind (Type)

type Method = T.Text

makeTeleReq :: (MonadHttp m, FromJSON a) =>
  Token -> Method -> [(T.Text, T.Text)] -> m (Either String a)
makeTeleReq token method params = do
  cont <- responseBody <$> sendRequest
  return $ parseResult cont
  where
    sendRequest = req 
      GET 
      (https "api.telegram.org" /: ("bot" `T.append` token) /: method) 
      NoReqBody 
      jsonResponse 
      (buildRequestParams params)
    parseResult response =
      case parseEither parseJSON response of
        Right (Response True _ (Just result)) -> Right result
        Right (Response False (Just errorMessage) _) ->
          Left errorMessage
        Right (Response True errorMessage Nothing) ->
          Left $ "Response was got, but body is empty, error: " ++ show errorMessage
        Left errorMessage -> Left errorMessage

    buildRequestParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
    buildRequestParams [] = mempty
    buildRequestParams params = mconcat $ fmap (uncurry (=:)) params  
  
-- handleResponse :: (Monad m, HttpResponse a, FromJSON a) => JsonResponse a -> m (Either String a)
-- handleResponse resp = do
--   let 
--     body = responseBody resp
--     res = parseJSON body
      
--   return (Left "")

main = do
  config <- fetchConfig
  logOut <- newTVarIO ""
  let
    decodeMessage :: BS.ByteString -> Either String Message
    decodeMessage = eitherDecode
    decodeUpdate :: BS.ByteString -> Either String Update
    decodeUpdate = eitherDecode
    decodeResponseUpdates :: BS.ByteString -> Either String (Response [Update])
    decodeResponseUpdates = eitherDecode
  response <- decodeMessage <$> BS.readFile "temp/messageText.json"
  incoming <- case response of 
    Left er -> error er
    Right incoming -> return incoming
  let incomingContent = case incoming & messageContent of
        MessageContentText msg -> msg
        _ -> error "Not text"
      query = [("chat_id", T.pack . show $ incoming & messageChat & chatId), ("text", T.pack incomingContent)]

  response <- runReq defaultHttpConfig (makeTeleReq (config & appConfigToken) "sendMessage" query) :: IO (Either String Message)
  either (\_ -> return ()) print response
  -- logged <- newTVarIO ""
  -- currentUp <- newTVarIO 1
  -- let
  --   myLogger :: LogLevel -> Text -> IO ()
  --   myLogger lvl text = atomically $ modifyTVar' logged (\s -> s `append` ( pack . show $ lvl) `append` ": " `append` text `append` "\n")
  --   myEnv = Env {
  --     logger = myLogger,
  --     logLevel = Debug
  --   }
  -- runReaderT (logDebug "123" >> logInfo "info") myEnv
  -- readTVarIO logged >>= \t -> mapM_ print (T.lines t)  

