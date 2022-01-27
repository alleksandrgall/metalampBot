module Req.Internal where
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import           Internal.Types         (Token)
import           Network.HTTP.Client    (ManagerSettings)
import           Network.HTTP.Req       (GET (GET), NoReqBody (NoReqBody),
                                         QueryParam, defaultHttpConfig, https,
                                         lbsResponse, req, responseBody,
                                         responseStatusCode,
                                         responseStatusMessage, runReq, (/:),
                                         (=:))



makeRequestReq :: (MonadThrow m, MonadIO m) => ManagerSettings -> Text -> Token -> Text -> [(Text, Text)] -> m B.ByteString
makeRequestReq _ url token method params = do
    rs <- runReq defaultHttpConfig (req
        GET
        builtUrl
        NoReqBody
        lbsResponse
        queryParams)
    -- case (responseStatusCode rs, responseStatusMessage rs, responseBody rs) of

    return (responseBody rs)
    where
        builtUrl = https url /: pack ("bot" ++ token) /: method
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
        is2XX x = x `mod` 200

