{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Module describes simple functionality for a Handle designed for making requests to REST APIs -}
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
import           Data.Aeson           (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Function        ((&))
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.String          (IsString (fromString))
import           Data.Text            (Text, intercalate, pack)
import           Exceptions.Body      (BodyException (..))
import           Exceptions.Parse     (ParseException (..))
import           Exceptions.Web       (WebException (..))
import qualified Handlers.Logger      as L
import           Internal.Types       (Protocol, Token, Url)
import           Network.HTTP.Client  (ManagerSettings, defaultManagerSettings)
import           Network.HTTP.Req     (BsResponse, HttpResponse, LbsResponse,
                                       responseBody, responseStatusCode,
                                       responseStatusMessage)
import           Prelude              hiding (error)

-- | Config for Web.Handler
--
-- | ManagerSettings does not add dep footprint since http-client is already in req's deps
data Config = Config {
   -- | Connection manager. Added for forward compatability
    cManagerSettings :: Maybe ManagerSettings
    -- | Bot token
  , cUrl             :: Text
    -- | Http or Https
  , cProtocol        :: Protocol
}

class Result a where
    getCode :: a -> Int
    getDescription :: a -> Text
    getBody :: a -> B.ByteString

instance Result LbsResponse  where
  getCode        = responseStatusCode
  getDescription = pack . show . responseStatusMessage
  getBody        = responseBody

data Handle m a b = Handle {
    hConfig      :: Config
  , hLogger      :: L.Handle m
  {-| Main function for making requests.
      Can throw WebException or SomeException with HttpException inside.

      Type a is for a result of a request.

      Type b is for a possible request body.
      Since there is only one possibility (the keyboard) it doesn't hurt.
  -}
  , hSendRequest ::
        (Result a, ToJSON b, MonadThrow m) =>
        Maybe ManagerSettings
        -> Maybe b
        -> Protocol
        -> Url
        -> Text
        -> [(Text, Text)]
        -> m a
}

-- | Exception type which makeRequest throws into the bot's logic
data RequestException = RWebException WebException | RParseException ParseException | RBodyException BodyException
    deriving (Show)
instance Exception RequestException where


-- | Function for making requests and parsing them
--
-- | Throws RequestException
makeRequest :: (FromJSON response, MonadCatch m, Result a, ToJSON b) => Handle m a b -> Maybe b -> Text -> [(Text, Text)] -> m response
makeRequest h@Handle {..} maybeBody method params = do
    resp <- handleWebException hLogger (hConfig & cUrl) method $ hSendRequest
        (hConfig & cManagerSettings)
        maybeBody
        (hConfig & cProtocol)
        (hConfig & cUrl)
        method
        params
    L.info hLogger $ L.WithBs
        ("Got response from" <> targetUrl <>
        "\n\tCode: " <> (pack . show $ resp & getCode) <>
        "\n\tDescription: " <> (resp & getDescription) <>
        "\n\tBody: ")
        (resp & getBody)
    if mempty == (resp & getBody) then do
        L.error hLogger $ L.JustText $
            "Response was got but body is empty from " <> targetUrl <>
            "\n\tCode: " <> (pack . show $ resp & getCode) <>
            "\n\tDescription: " <> (resp & getDescription)
        throwM $ RBodyException . EmptyReponseBody  $
            "\nCode: " <> (pack . show $ resp & getCode) <> "\nDescription: " <> (resp & getDescription)
    else case eitherDecode (resp & getBody) of
        Right body -> return body
        Left e     -> do
            L.error hLogger $ L.WithBs ("Parsing failed due to mismatching type, error:\n\t" <> fromString e) (resp & getBody)
            throwM $ RParseException . WrongType . fromString $ e
    where targetUrl = (hConfig & cUrl) <> "/" <> method <> "/" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ params)



-- | Just rethrowing exceptions in the form of WebException for the bot logic to deal with
handleWebException :: (MonadCatch m) => L.Handle m -> Url -> Text -> m a -> m a
handleWebException hLogger url method = handleFromException . handlePure
    where
        logException NoResponse = L.error hLogger (L.JustText $ "No response from " <> url)
        logException (CodeMessageException c t) = L.error hLogger (L.JustText $ "Server answered with an error: " <> (pack . show $ c) <> ". Desctiption: " <> t)
        logException (ConnectionException t) = L.error hLogger (L.JustText $ "Unnable to connect: " <> t)
        logException (InvalidUrlException url msg) = L.error hLogger (L.JustText $ "Url is invalid: " <> url <> ", error: " <> msg)
        logException (SomeWebException se) = L.error hLogger (L.JustText $ "Web exception was caught: " <> (pack . show $ se))

        handlePure = handle $ \e -> do
            logException e
            throwM $ RWebException e

        handleFromException = handle $ \e -> do
            let maybeWebException = fromException e
            case maybeWebException of
                Just webE -> do
                    logException webE
                    throwM $ RWebException webE
                -- In case of parse fromException failing throws SomeException
                Nothing -> throwM e






