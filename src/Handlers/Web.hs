{-# LANGUAGE RankNTypes #-}
module Handlers.Web where

import Network.HTTP.Client ( ManagerSettings )
import qualified Logger as L
import Data.Text as T (Text, append)
import Data.Aeson (FromJSON)
import Control.Monad.Catch (MonadThrow (throwM), MonadCatch (catch), Exception)
import Data.Function ((&))
import qualified Data.ByteString.Char8 as B

type Token = String

data Config = Config {
    -- | ManagerSettings does not add dep footprint since http-client is already in req's deps
    -- added for forward compatability
    cManagerSettings :: ManagerSettings,
    cToken :: Token,
    cUrl :: Text
}

data Handle m = Handle {
    hConfig :: Config
  , hLogger :: L.Handle m
  , hMakeRequest :: (MonadThrow m) => ManagerSettings -> Token -> Text -> [(Text, Text)] -> m B.ByteString
}

data WebException = NoResponse Text |  EmptyReponseBody Text deriving (Show)

instance Exception WebException


makeRequest :: (FromJSON a, Show a, MonadCatch m) => Handle m -> Text -> [(Text, Text)] -> m a
makeRequest h method params = do
    handleException
        (do
            bs <- (h & hMakeRequest)
                (h & hConfig & cManagerSettings) (h & hConfig & cToken) ((h & hConfig & cUrl) `append` "/" `append` method) params

            return undefined)
        (\e -> return undefined )

handleException :: (MonadCatch m) => m a -> (WebException -> m a) -> m a
handleException = catch
-- makeRequest h token url params = do
--     catch 
--         (do 
--             response <- (h & hMakeRequest) token url params
--             L.info (h & hLogger) "" 
--             return response)
--         (handleException h)
--     where
--         handleException :: Handle a m -> WebException -> m a
--         handleException h e = undefined