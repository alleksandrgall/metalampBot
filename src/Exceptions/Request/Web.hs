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
data WebException = CodeMessageException Int Text | ConnectionException Text | InvalidUrlException Text Text
    deriving (Show)

-- | fromException for exceptions from Network.Http.Client
instance Exception WebException where
    fromException se |
        Just httpException <- fromException se = case httpException of
            VanillaHttpException (HttpExceptionRequest _ eContent) -> case eContent of
                (StatusCodeException rs _) -> Just (CodeMessageException (respStatusCode rs) (respStatusMessage rs))
                ConnectionFailure e -> Just $ ConnectionException . pack . show $ e
                ConnectionTimeout -> Just $ ConnectionException mempty
                other -> Nothing
            VanillaHttpException (Http.InvalidUrlException url msg) -> Just $ Exceptions.Request.Web.InvalidUrlException (pack url) $ pack msg
            _ -> Nothing
        | otherwise = Nothing

        where
            respStatusCode rs = rs & responseStatus & statusCode
            respStatusMessage rs = pack . show $ rs & responseStatus & statusMessage
