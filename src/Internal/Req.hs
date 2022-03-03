{-# LANGUAGE RankNTypes #-}
module Internal.Req (makeRequest, parseResponse) where

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
import           Internal.Types         (Token)
import           Network.HTTP.Client    (ManagerSettings)
import           Network.HTTP.Req       (GET (GET), LbsResponse,
                                         NoReqBody (NoReqBody), POST (POST),
                                         QueryParam, ReqBodyJson (ReqBodyJson),
                                         defaultHttpConfig, http, https,
                                         lbsResponse, req, responseBody,
                                         responseStatusCode,
                                         responseStatusMessage, runReq, (/:),
                                         (=:))

parseResponse :: (FromJSON response, MonadThrow m, Show response) => L.Handle m -> B.ByteString -> m response
parseResponse hLogger respBody = case eitherDecode respBody of
    Right result -> do
        L.debug hLogger (L.JustText (pack . show $ result))
        return result
    Left e     -> do
        L.error hLogger $ L.WithBs ("Parsing failed due to mismatching type, error:\n\t" <> fromString e) respBody
        throwM $ RParseException . WrongType . fromString $ e

makeRequest :: (ToJSON a, MonadIO m) =>
    L.Handle m -> Maybe a -> Text -> [Text] -> [(Text, Text)] -> m B.ByteString
makeRequest hLogger maybeBody url methods params = do
    resp <- sendRequestReq
        maybeBody
        url
        methods
        params
    L.debug hLogger $ L.WithBs
        ("Got response from" <> targetUrl <>
        "\n\tCode: " <> (pack . show $ resp & responseStatusCode) <>
        "\n\tDescription: " <> (resp & pack . show . responseStatusMessage) <>
        "\n\tBody: ")
        (resp & responseBody)
    return (resp & responseBody)
    where targetUrl = url <> "/" <> intercalate "/" methods <> "?" <> (intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ params)


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
        builtHttp = foldl (/:) (http url) methods
        builtHttps = foldl (/:) (https url) methods
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
