module Internal.Req (makeRequest) where

import           Control.Monad.Catch    (Exception (fromException), MonadCatch,
                                         MonadThrow, handle, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (FromJSON (parseJSON), ToJSON,
                                         Value (Object), eitherDecode, (.:))
import qualified Data.ByteString.Lazy   as B
import           Data.Function          ((&))
import           Data.String            (IsString (fromString))
import           Data.Text              (Text, intercalate, pack)
import           Exceptions.Request
import qualified Handlers.Logger        as L
import           Internal.Types         (Protocol (..), Token)
import           Network.HTTP.Client    (ManagerSettings)
import           Network.HTTP.Req       (GET (GET), LbsResponse,
                                         NoReqBody (NoReqBody), POST (POST),
                                         QueryParam, ReqBodyJson (ReqBodyJson),
                                         ReqBodyLbs (ReqBodyLbs),
                                         defaultHttpConfig, http, https,
                                         lbsResponse, req, responseBody,
                                         responseStatusCode,
                                         responseStatusMessage, runReq, (/:),
                                         (=:))

newtype Result a = Result a
instance (FromJSON a) => FromJSON (Result a) where
    parseJSON (Object o) = Result <$> o .: "result"
    parseJSON _          = mempty

makeRequest :: (FromJSON response, MonadCatch m, ToJSON b, MonadIO m) =>
    L.Handle m -> Maybe b -> Protocol -> Text -> [Text] -> [(Text, Text)] -> m response
makeRequest hLogger maybeBody p url methods params = do
    resp <- handleWebException hLogger url methods $ sendRequestReq
        maybeBody
        p
        url
        methods
        params
    L.info hLogger $ L.WithBs
        ("Got response from" <> targetUrl <>
        "\n\tCode: " <> (pack . show $ resp & responseStatusCode) <>
        "\n\tDescription: " <> (resp & pack . show . responseStatusMessage) <>
        "\n\tBody: ")
        (resp & responseBody)
    if mempty == (resp & responseBody) then do
        L.error hLogger $ L.JustText $
            "Response was got but body is empty from " <> targetUrl <>
            "\n\tCode: " <> (pack . show $ resp & responseStatusCode) <>
            "\n\tDescription: " <> (resp & pack . show . responseStatusMessage)
        throwM $ RBodyException EmptyReponseBody
    else case eitherDecode (resp & responseBody) of
        Right (Result body) -> return body
        Left e     -> do
            L.error hLogger $ L.WithBs ("Parsing failed due to mismatching type, error:\n\t" <> fromString e) (resp & responseBody)
            throwM $ RParseException . WrongType . fromString $ e
    where targetUrl = url <> "/" <> intercalate "/" methods <> "?" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ params)



-- | Just rethrowing exceptions in the form of WebException for the bot logic to deal with
handleWebException :: (MonadCatch m) => L.Handle m -> Text -> [Text] -> m a -> m a
handleWebException hLogger url methods = handleFromException . handlePure
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

-- | Function to make requests using Network.HTTP.Req library
--
-- | Can throw Exceptions from Network.HTTP.Client
sendRequestReq :: (MonadThrow m, MonadIO m, ToJSON b) =>
    Maybe b -> -- | possible request body
    Protocol ->
    Text -> -- | base api url
    [Text] -> -- | method
    [(Text, Text)] -> -- | method params
    m LbsResponse
sendRequestReq maybeBody prot url methods params =
    if prot == Https then requestHttps else requestHttp
    where
        -- | Don't know how to avoid code duplication since type sigs must be different, must consult the CHAT
        requestHttp =
         maybe
            (runReq defaultHttpConfig (req
                GET
                builtHttp
                NoReqBody
                lbsResponse
                queryParams))
            (\body -> runReq defaultHttpConfig (req
                POST
                builtHttp
                (ReqBodyJson body)
                lbsResponse
                queryParams))
            maybeBody
        requestHttps =
         maybe
            (runReq defaultHttpConfig (req
                GET
                builtHttps
                NoReqBody
                lbsResponse
                queryParams))
            (\body -> runReq defaultHttpConfig (req
                POST
                builtHttps
                (ReqBodyJson body)
                lbsResponse
                queryParams))
            maybeBody
        builtHttp = foldl (/:) (http url) methods
        builtHttps = foldl (/:) (https url) methods
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
