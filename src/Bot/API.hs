{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Bot.API where

import Network.HTTP.Req
import Bot.ResponseTypes
import Control.Monad.Reader 
import Config
import Data.Text ( Text )
import Data.Aeson (FromJSON (parseJSON))
import Control.Exception (throwIO)
import Control.Monad.RWS
import Data.Aeson.Types (parseEither)

type Method = Text

class MonadBot m where
    makeReq :: (FromJSON a) => Method -> [(Text, Text)] -> m (Either String a)

-- newtype (MonadTrans m) => App m a = App {unApp :: RWST AppConfig [(Text,Text)] () (m IO) a}
--     deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadWriter [(Text, Text)], MonadState ())

instance (Monad m, MonadRWS AppConfig [(Text, Text)] () m, MonadHttp m) => MonadBot m where
    makeReq method params = do
        messenger <- asks appConfigMessenger
        case messenger of
            Tele -> do
                token <- asks appConfigToken
                let buildUrl = http "api.telegram.org" /: ("bot" <> token) /: method
                    queryParams = buildRequestParams params
                    parseResult response =
                      case parseEither parseJSON response of
                        Right (Update True (Just result)) -> Right result
                        Right (Update False  _) ->
                          Left "Error"
                        Left errorMessage -> Left errorMessage

                resp <- responseBody <$> req GET buildUrl NoReqBody jsonResponse queryParams
                return . parseResult $ resp

            _ -> undefined 

-- instance MonadBot (ReaderT AppConfig IO) where
--     makeReq method params = do
--        mes <- asks appConfigMessenger
--        case mes of 
--            Tele -> do
--                token <- asks appConfigToken 
--                let buildUrl = http "api.telegram.org" /: ("bot" <> token) /: method
--                    queryParams = buildRequestParams params
--                resp <- req GET buildUrl NoReqBody jsonResponse queryParams 
--                undefined 
--            _ -> return $ Left "Vk not emplimented"

buildRequestParams :: (QueryParam p, Monoid p) => [(Text, Text)] -> p
buildRequestParams [] = mempty
buildRequestParams params = mconcat $ fmap (uncurry (=:)) params
