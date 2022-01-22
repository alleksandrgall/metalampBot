{-# LANGUAGE RankNTypes #-}
module Handlers.Web
    (Config(..)
    ,Handle(..)
    ,WebException(..)
    ,makeRequest)
    where

import           Control.Monad.Catch        (Exception, MonadCatch (catch),
                                             MonadThrow (throwM))
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function              ((&))
import           Data.String                (IsString (fromString))
import           Data.Text                  as T (Text)
import qualified Handlers.Logger            as L
import           Network.HTTP.Client        (ManagerSettings)
import           Prelude                    hiding (error)

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

data WebException = NoResponse | EmptyReponseBody Text | SomeWebException Text deriving (Show)
instance Exception WebException

newtype ParseException = WrongType Text deriving (Show)
instance Exception ParseException

data ResponseException = RWebException WebException | RParseException ParseException deriving (Show)
instance Exception ResponseException

-- | Function for making requests, parsing them and throwing exceptions
makeRequest :: (FromJSON a, Show a, MonadCatch m) => Handle m -> Text -> [(Text, Text)] -> m a
makeRequest h method params = do
    bs <- handleWebException h url $(h & hMakeRequest)
                (h & hConfig & cManagerSettings) (h & hConfig & cToken) url params
    L.info (h & hLogger) $ L.WithBs ("Got response " <> url) bs
    case eitherDecode bs of
        Right response -> return response
        Left e         -> throwM $ RParseException (WrongType . fromString $ e)

    where url = (h & hConfig & cUrl) <> "/" <> method


-- | Just rethrowing exceptions for bot logic to deal with
handleWebException :: (MonadCatch m) => Handle m -> Text -> m a -> m a
handleWebException h url = flip catch (\e -> case e of
            NoResponse -> do
                L.error (h & hLogger) (L.JustText $ "No response from " <> url)
                throwM (RWebException e)
            EmptyReponseBody t -> do
                L.error (h & hLogger) (L.JustText $ "Response from " <> url <> " was got but body was empty, error: " <> t)
                throwM (RWebException e)
            SomeWebException t -> do
                L.error (h & hLogger) (L.JustText $ "Response from " <> url <> " was an error :" <> t)
                throwM (RWebException e)
            )


