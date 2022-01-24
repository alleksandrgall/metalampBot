{-# LANGUAGE LambdaCase #-}
module Req.Internal where

import           Control.Monad.Catch    (MonadCatch (catch),
                                         MonadThrow (throwM))
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as B
import           Data.Function          ((&))
import           Data.Text              (Text, pack)
import qualified Handlers.Logger        as L
import qualified Handlers.Web           as Web
import           Internal.Types         (Token)
import           Network.HTTP.Client    (HttpException (HttpExceptionRequest, InvalidUrlException),
                                         HttpExceptionContent (StatusCodeException),
                                         ManagerSettings,
                                         Response (responseStatus))
import           Network.HTTP.Req
import           Network.HTTP.Types     (Status (statusCode, statusMessage))


makeRequestReq :: (MonadCatch m, MonadIO m) => L.Handle m -> ManagerSettings -> Text -> Token -> Text -> [(Text, Text)] -> m B.ByteString
makeRequestReq hL _ url token method params = do
    catch
        (responseBody <$> runReq defaultHttpConfig (req
            GET
            builtUrl
            NoReqBody
            lbsResponse
            queryParams))
        (\case
        VanillaHttpException (HttpExceptionRequest _ eContent) ->
          case eContent of
            StatusCodeException rs _ -> do
                L.error hL $ L.JustText $
                    "Server answered with an error code: " <> (pack . show $ respStatusCode rs) <>
                    ".\nDesctiption: \n\t" <> respStatusMessage rs
                throwM (Web.CodeMessageException (respStatusCode rs) (respStatusMessage rs))  -- server answered with anything but 2XX
            other -> do
                L.error hL $ L.JustText $
                    "Some web exception was caught, error:\n\t" <> (pack . show $ eContent)
                throwM Web.SomeWebException
        VanillaHttpException (InvalidUrlException url msg) -> do
                L.error hL $ L.JustText $
                    "Url " <> pack url <> " is invalid, error: " <> pack msg
                throwM Web.SomeWebException
        JsonHttpException s -> do
                L.error hL $ L.JustText $
                    "Json exception:\n\t" <> pack s
                throwM Web.SomeWebException
        )
    --todo Проспаться и переделать скорее всего
    --возвращать не респонс боди, а респонс из катча и обрабатывать штуки
    --Может быть кастомный инстанс handleHttpException будет более в тему
    where
        respStatusCode rs = rs & responseStatus & statusCode
        respStatusMessage rs = pack . show $ rs & responseStatus & statusMessage
        builtUrl = https url /: pack ("bot" ++ token) /: method
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params

