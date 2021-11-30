{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

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
import Control.Monad.Except
import Control.Monad.State


type Method = Text

class (Monad m) => MonadBot m where
    makeReq :: (FromJSON a) => Method -> [(Text, Text)] -> m (Either String a)
    

-- newtype App t a = App {unApp :: ReaderT AppConfig (StateT )}
--     deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadWriter [(Text, Text)], MonadState ())

-- newtype App a = App {unApp :: ReaderT AppConfig (StateT Int ())}
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
