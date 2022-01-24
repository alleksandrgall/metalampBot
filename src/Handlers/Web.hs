{-# LANGUAGE RankNTypes #-}
module Handlers.Web
    (Config(..)
    ,Handle(..)
    ,WebException(..)
    ,ParseException(..)
    ,Token
    ,makeRequest)
    where

import           Control.Monad.Catch        (Exception, MonadCatch (catch),
                                             MonadThrow (throwM), handle)
import           Data.Aeson                 (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function              ((&))
import           Data.Maybe                 (fromMaybe)
import           Data.String                (IsString (fromString))
import           Data.Text                  as T (Text)
import qualified Handlers.Logger            as L
import           Internal.Types             (Token, Url)
import           Network.HTTP.Client        (ManagerSettings,
                                             defaultManagerSettings)
import           Prelude                    hiding (error)

-- | Config for Web.Handler
--
-- | ManagerSettings does not add dep footprint since http-client is already in req's deps
data Config = Config {
    -- | Connection manager. Added for forward compatability
    cManagerSettings :: Maybe ManagerSettings
    -- | Bot token
  , cToken           :: Token
    -- | Base URL of mesenger's API
  , cUrl             :: Text
}

data Handle m = Handle {
    hConfig      :: Config
  , hLogger      :: L.Handle m
  -- | Main function for making requests.
  --
  -- Can throw WebException
  , hMakeRequest :: (MonadThrow m) => ManagerSettings -> Url -> Token -> Text -> [(Text, Text)] -> m B.ByteString
}


-- | Exception type which hMakeRequest could throw
data WebException = CodeMessageException Int Text | NoResponse | EmptyReponseBody Text | SomeWebException
    deriving (Show)
instance Exception WebException

-- | Exception type for parse error
newtype ParseException = WrongType Text
    deriving (Show)
instance Exception ParseException

-- | Exception type which makeRequest throws into the bot's logic
data ResponseException = RWebException WebException | RParseException ParseException
    deriving (Show)
instance Exception ResponseException

-- | Function for making requests, parsing them and throwing exceptions
makeRequest :: (FromJSON a, Show a, MonadCatch m) => Handle m -> Text -> [(Text, Text)] -> m a
makeRequest h@Handle {..} method params = do
    bs <- handleWebException h (hConfig & cUrl) method $ hMakeRequest
        (fromMaybe defaultManagerSettings $ hConfig & cManagerSettings)
        (hConfig & cUrl)
        (hConfig & cToken)
        method
        params
    L.info hLogger $ L.WithBs ("Got response " <> (hConfig & cUrl) <> method) bs
    case eitherDecode bs of
        Right response -> return response
        Left e         -> do
            L.error hLogger (L.JustText $ "Parsing failed due to mismatching type, error:\n\t" <> fromString e)
            throwM $ RParseException . WrongType . fromString $ e




-- | Just rethrowing exceptions for bot logic to deal with
handleWebException :: (MonadCatch m) => Handle m -> Url -> Text -> m a -> m a
handleWebException h url method = handle $ \e -> case e of
    NoResponse -> do
        L.error (h & hLogger) (L.JustText $ "No response from " <> url)
        throwM (RWebException e)
    EmptyReponseBody t -> do
        L.error (h & hLogger) (L.JustText $ "Response from " <> url <> " was got but body was empty, error:\n\t" <> t)
        throwM (RWebException e)
    SomeWebException -> throwM (RWebException e)
    CodeMessageException c t -> throwM (RWebException e)
