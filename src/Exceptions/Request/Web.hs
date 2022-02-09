{-# LANGUAGE ScopedTypeVariables #-}
module Exceptions.Request.Web
    (WebException(..))
where
import           Control.Exception   (Exception, SomeException)
import           Control.Monad.Catch (Exception (fromException),
                                      MonadThrow (throwM))
import           Data.Function
import           Data.Text           (Text, pack)
import           Network.HTTP.Client (HttpException (HttpExceptionRequest),
                                      HttpExceptionContent (ConnectionFailure, ConnectionTimeout, StatusCodeException),
                                      Response (responseStatus))
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Req    (HttpException (VanillaHttpException))
import           Network.HTTP.Types  (Status (statusCode, statusMessage))


-- | Representation of different exceptions request could result in
data WebException = CodeMessageException Int Text | NoResponse | SomeWebException SomeException
    | ConnectionException Text | InvalidUrlException Text Text
    deriving (Show)

-- | fromException for exceptions from Network.Http.Client
instance Exception WebException where
    fromException se |
        Just httpException <- fromException se = case httpException of
            VanillaHttpException (HttpExceptionRequest _ eContent) -> case eContent of
                (StatusCodeException rs _) -> throwM (CodeMessageException (respStatusCode rs) (respStatusMessage rs))
                ConnectionFailure e -> throwM . ConnectionException . pack . show $ e
                ConnectionTimeout -> throwM . ConnectionException $ mempty
                other -> throwM . SomeWebException $ se
            VanillaHttpException (Http.InvalidUrlException url msg) -> throwM . Exceptions.Request.Web.InvalidUrlException (pack url) $ pack msg
            other -> throwM . SomeWebException $ se
        | otherwise = Nothing

        where
            respStatusCode rs = rs & responseStatus & statusCode
            respStatusMessage rs = pack . show $ rs & responseStatus & statusMessage
