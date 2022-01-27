{-# LANGUAGE RankNTypes #-}
module Handlers.Web
    (Config(..)
    ,Handle(..)
    ,ParseException(..)
    ,Result
    ,makeRequest
    ,getCode
    ,getBody
    ,getDescription)
    where


import           Control.Monad.Catch  (Exception, MonadCatch,
                                       MonadThrow (throwM), fromException,
                                       handle, toException)
import           Data.Aeson           (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Function        ((&))
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.String          (IsString (fromString))
import           Data.Text            (Text, pack)
import           Exceptions.Body
import           Exceptions.Parse
import           Exceptions.Web
import qualified Handlers.Logger      as L
import           Internal.Types       (Token, Url)
import           Network.HTTP.Client  (ManagerSettings, defaultManagerSettings)
import           Prelude              hiding (error)

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

class Result a where
    getCode :: a -> Int
    getDescription :: a -> Text
    getBody :: a -> B.ByteString


data Handle a m = Handle {
    hConfig      :: Config
  , hLogger      :: L.Handle m
  -- | Main function for making requests.
  --
  -- Can throw WebException
  , hMakeRequest :: (Result a, MonadThrow m) => Maybe ManagerSettings -> Url -> Token -> Text -> [(Text, Text)] -> m a
}

-- | Exception type which makeRequest throws into the bot's logic
data RequestException = RWebException WebException | RParseException ParseException | RBodyException BodyException
    deriving (Show)
instance Exception RequestException where


-- | Function for making requests, parsing them and throwing exceptions
makeRequest :: (FromJSON a, MonadCatch m, Result b) => Handle b m -> Text -> [(Text, Text)] -> m a
makeRequest h@Handle {..} method params = do
    resp <- handleWebException hLogger (hConfig & cUrl) method $ hMakeRequest
        (hConfig & cManagerSettings)
        (hConfig & cUrl)
        (hConfig & cToken)
        method
        params
    L.info hLogger $ L.WithBs ("Got response " <> (hConfig & cUrl) <> method <>
        "\nCode: " <> (pack . show $ resp & getCode) <>
        "\nDescription: " <> (resp & getDescription) <>
        "\nBody: ") (resp & getBody)
    if mempty == (resp & getBody) then do
        L.error hLogger $ L.JustText $ "Response was got but body is empty" <> (hConfig & cUrl) <> method <>
            "\nCode: " <> (pack . show $ resp & getCode) <>
            "\nDescription: " <> (resp & getDescription)
        throwM $ toException . RBodyException . EmptyReponseBody  $ "\nCode: " <> (pack . show $ resp & getCode) <> "\nDescription: " <> (resp & getDescription)
    else case eitherDecode (resp & getBody) of
        Right body -> return body
        Left e         -> do
            L.error hLogger (L.JustText $ "Parsing failed due to mismatching type, error:\n\t" <> fromString e)
            throwM $ toException . RParseException . WrongType . fromString $ e




-- | Just rethrowing exceptions for bot logic to deal with
--
-- Написать через fromException и выкидывать через toException
handleWebException :: (MonadCatch m) => L.Handle m -> Url -> Text -> m a -> m a
handleWebException hLogger url method = handle $ \e -> do
    let maybeWebException = fromException e
        toSome = toException . RWebException . fromJust
    case maybeWebException of
        Just NoResponse -> L.error hLogger (L.JustText $ "No response from " <> url)
        -- maybe todo 3XX
        Just (CodeMessageException c t) -> L.error hLogger (L.JustText $ "Server answered with an error: " <> (pack . show $ c) <> ". Desctiption: " <> t)
        Just (ConnectionException t) -> L.error hLogger (L.JustText $ "Unnable to connect: " <> t)
        Just (InvalidUrlException url msg) -> L.error hLogger (L.JustText $ "Url is invalid: " <> url <> ", error: " <> msg)
        Just (SomeWebException se) -> L.error hLogger (L.JustText $ "Web exception was caught: " <> (pack . show $ se))
        Nothing -> L.error hLogger (L.JustText $ "Web exception was caught: " <> (pack . show $ e)) >> throwM e
    throwM (toSome maybeWebException)



