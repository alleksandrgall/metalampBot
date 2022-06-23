{-# LANGUAGE RankNTypes #-}

module Internal.Req (makeRequest, parseResponse) where

import Control.Monad.Catch
  ( MonadCatch,
    MonadThrow (..),
  )
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Function ((&))
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Exceptions.Request
import qualified Handlers.Logger as L
import Internal.ShowText (showText)
import Network.HTTP.Req

parseResponse :: (FromJSON response, MonadThrow m, Show response) => L.Handle m -> B.ByteString -> m response
parseResponse hLogger respBody = case eitherDecode respBody of
  Right result -> do
    L.debug hLogger (showText result)
    return result
  Left e -> do
    L.error hLogger $ ("Parsing failed due to mismatching type, error:\n\t" <> fromString e <> "\n") <> (T.pack . BC.unpack $ respBody)
    throwM $ RParseException . WrongType . fromString $ e

makeRequest ::
  (ToJSON a, MonadIO m, MonadCatch m) =>
  L.Handle m ->
  Maybe a ->
  Text ->
  [Text] ->
  [(Text, Text)] ->
  m B.ByteString
makeRequest hLogger maybeBody url methods params = do
  resp <- sendRequestReq maybeBody url methods params
  L.debug hLogger $
    ( "Got response from " <> targetUrl
        <> "\n\tCode: "
        <> showText (resp & responseStatusCode)
        <> "\n\tDescription: "
        <> showText (resp & responseStatusMessage)
        <> "\n\tBody: "
    )
      <> (T.pack . BC.unpack $ resp & responseBody)
  return (resp & responseBody)
  where
    targetUrl = url <> "/" <> T.intercalate "/" methods <> "?" <> (T.intercalate "&" . map (\(k, v) -> k <> "=" <> v) $ params)

-- | Function to make requests using Network.HTTP.Req library
-- | Can throw Exceptions from Network.HTTP.Client
sendRequestReq ::
  (MonadIO m, ToJSON b) =>
  Maybe b ->
  -- | possible request body
  Text ->
  -- | base api url
  [Text] ->
  -- | method
  [(Text, Text)] ->
  -- | method params
  m LbsResponse
sendRequestReq maybeBody url methods params =
  maybe
    ( runReq
        defaultHttpConfig
        ( req
            GET
            builtHttps
            NoReqBody
            lbsResponse
            queryParams
        )
    )
    ( \body ->
        runReq
          defaultHttpConfig
          ( req
              POST
              builtHttps
              (ReqBodyJson body)
              lbsResponse
              queryParams
          )
    )
    maybeBody
  where
    builtHttps = foldl (/:) (https url) methods
    queryParams :: (QueryParam p, Monoid p) => p
    queryParams = mconcat $ fmap (uncurry (=:)) params
