{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handlers.Web where

import           Control.Monad.Catch   (Exception, MonadCatch (catch),
                                        MonadThrow (throwM))
import           Data.Aeson            (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function         ((&))
import           Data.Text             as T (Text)
import qualified Handlers.Logger as L 
import           Network.HTTP.Client   (ManagerSettings)
import Data.String (IsString(fromString))
import Prelude hiding (error)

type Token = String

-- | ManagerSettings does not add dep footprint since http-client is already in req's deps
data Config = Config {
    -- | Added for forward compatability
    cManagerSettings :: ManagerSettings
  , cToken           :: Token
  , cUrl             :: Text
}

data Handle m = Handle {
    hConfig      :: Config
  , hLogger      :: L.Handle m
  -- | Main function for making requests
  , hMakeRequest :: (MonadThrow m) => ManagerSettings -> Token -> Text -> [(Text, Text)] -> m B.ByteString
}

data WebException = NoResponse | EmptyReponseBody Text | SomeWebException Text | WrongType Text deriving (Show)

instance Exception WebException

-- | Function for making requests, parsing them and throwing exceptions
makeRequest :: (FromJSON a, Show a, MonadCatch m) => Handle m -> Text -> [(Text, Text)] -> m a
makeRequest h method params = do
    handleException h url $ do
            bs <- (h & hMakeRequest)
                (h & hConfig & cManagerSettings) (h & hConfig & cToken) url params
            L.info (h & hLogger) $ L.WithBs ("Got response " <> url) bs
            case eitherDecode bs of
                Right response -> return response
                Left e -> throwM $ WrongType . fromString $ e

    where url = (h & hConfig & cUrl) <> "/" <> method


-- | Just rethrowing exceptions for bot logic to deal with
handleException :: (MonadCatch m) => Handle m -> Text -> m a -> m a
handleException h url = flip catch (\e -> case e of
            NoResponse -> do
                L.error (h & hLogger) (L.JustText $ "No response from " <> url)
                throwM e
            EmptyReponseBody t -> do
                L.error (h & hLogger) (L.JustText $ "Response from " <> url <> " was got but body is empty, error: " <> t)
                throwM e
            SomeWebException t -> do
                L.error (h & hLogger) (L.JustText $ "Response from " <> url <> " was an error :" <> t)
                throwM e
            WrongType t -> do
                 L.error (h & hLogger) (L.JustText $ "Parsing of response from " <> url <> " failed, error:" <> t)
                 throwM e
            )