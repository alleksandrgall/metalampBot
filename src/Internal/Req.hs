{-# LANGUAGE RankNTypes #-}
module Internal.Req (makeRequest, parseResponse) where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.Catch        (Exception (fromException, toException),
                                             MonadCatch, MonadThrow (..),
                                             handle)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Aeson                 (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Function              ((&))
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text, intercalate, unpack)
import           Exceptions.Request
import qualified Handlers.Logger            as L
import           Network.HTTP.Req           (GET (GET), LbsResponse,
                                             NoReqBody (NoReqBody), POST (POST),
                                             QueryParam,
                                             ReqBodyJson (ReqBodyJson),
                                             defaultHttpConfig, https,
                                             lbsResponse, req, responseBody,
                                             responseStatusCode,
                                             responseStatusMessage, runReq,
                                             (/:), (=:))
import           System.Exit                (exitFailure)

parseResponse :: (FromJSON response, MonadThrow m, Show response) => L.Handle m -> B.ByteString -> m response
parseResponse hLogger respBody = case eitherDecode respBody of
    Right result -> do
        L.debug hLogger (show result)
        return result
    Left e     -> do
        L.error hLogger $ ("Parsing failed due to mismatching type, error:\n\t" <> fromString e <> "\n") <> BC.unpack respBody
        throwM $ RParseException . WrongType . fromString $ e

makeRequest :: (ToJSON a, MonadIO m, MonadCatch m) =>
    L.Handle m -> Maybe a -> Text -> [Text] -> [(Text, Text)] -> m B.ByteString
makeRequest hLogger maybeBody url methods params = do
    resp <- handleWeb hLogger targetUrl $ sendRequestReq maybeBody url methods params
    L.debug hLogger $
        ("Got response from " <> unpack targetUrl <>
        "\n\tCode: " <> show (resp & responseStatusCode) <>
        "\n\tDescription: " <> (resp & show . responseStatusMessage) <>
        "\n\tBody: ") <> BC.unpack (resp & responseBody)
    return (resp & responseBody)
    where targetUrl = url <> "/" <> intercalate "/" methods <> "?" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ params)

handleWeb :: (MonadIO m, MonadCatch m) => L.Handle m -> Text -> m a -> m a
handleWeb hL targetUrl m =
    handle (\e -> case fromException e of
    Just (CodeMessageException code _) -> do
        if code == 429 then do
            L.error hL (unpack $ "Error occured while requesting to: " <> targetUrl)
            L.error hL "To many requests, 25 seconds delay"
            liftIO $ threadDelay 25000
            m
        else throwM . toException $ e
    Just (ConnectionException t) -> do
        L.error hL (unpack $ "Error occured while requesting to: " <> targetUrl)
        L.error hL $ "Connection failure: " <> unpack t <> "\n 25 seconds delay"
        liftIO $ threadDelay 25000
        m
    Just (InvalidUrlException url mes) -> do
        L.error hL (unpack $ "Error occured while requesting to: " <> targetUrl)
        L.error hL $ "Invalid url: " <> unpack url <> "\n message: " <> unpack mes
        liftIO exitFailure
    Nothing -> throwM e) m

-- | Function to make requests using Network.HTTP.Req library
--
-- | Can throw Exceptions from Network.HTTP.Client
sendRequestReq :: (MonadIO m, ToJSON b) =>
    Maybe b -> -- | possible request body
    Text -> -- | base api url
    [Text] -> -- | method
    [(Text, Text)] -> -- | method params
    m LbsResponse
sendRequestReq maybeBody url methods params =
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
    where
        builtHttps = foldl (/:) (https url) methods
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
