{-# LANGUAGE RankNTypes #-}
module Handlers.Web where

import Network.HTTP.Client
import qualified Logger as L
import Data.Text (Text)
import Data.Aeson (FromJSON)
import Control.Monad.Catch (MonadThrow (throwM), MonadCatch (catch), Exception)
import Data.Function ((&))


type Token = String

data Config = Config {
    --ManagerSettings does not add dep footprint since http-client is already in req
    cManagerSettings :: ManagerSettings
}

data Handle a m = Handle {
    hConfig :: Config
  , hLogger :: L.Handle m
  , hMakeRequest :: (FromJSON a, MonadThrow m) => Token -> Text -> [(Text, Text)] -> m a
}

data WebException = NoResponse Text |  EmptyReponseBody Text deriving (Show)

instance Exception WebException

makeRequest :: (FromJSON a, Show a, MonadCatch m) => Handle a m -> Token -> Text -> [(Text, Text)] -> m a
makeRequest h token url params = do
    catch 
        (do 
            response <- (h & hMakeRequest) token url params
            L.info (h & hLogger) "" 
            return response)
        (handleException h) x``
    where
        handleException :: Handle a m ->  WebException -> m a
        handleException h e = undefined 