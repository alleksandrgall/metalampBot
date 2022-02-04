module Web.Req.Internal where
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (ToJSON)
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import           Internal.Types         (Token)
import           Network.HTTP.Client    (ManagerSettings)
import           Network.HTTP.Req       (GET (GET), LbsResponse,
                                         NoReqBody (NoReqBody), POST (POST),
                                         QueryParam, ReqBodyJson (ReqBodyJson),
                                         ReqBodyLbs (ReqBodyLbs),
                                         defaultHttpConfig, https, lbsResponse,
                                         req, runReq, (/:), (=:))

-- | Function to make requests using Network.HTTP.Req library
--
-- | Can throw Exceptions from Network.HTTP.Client
sendRequestReq :: (MonadThrow m, MonadIO m, ToJSON b) =>
    Maybe ManagerSettings ->
    Maybe b -> -- | possible request body
    Text -> -- | base api url
    Token ->
    Text -> -- | method
    [(Text, Text)] -> -- | method params
    m LbsResponse
sendRequestReq _ maybeBody url token method params =
    maybe
        (runReq defaultHttpConfig (req
            GET
            builtUrl
            NoReqBody
            lbsResponse
            queryParams))
        (\body -> runReq defaultHttpConfig (req
            POST
            builtUrl
            (ReqBodyJson body)
            lbsResponse
            queryParams))
        maybeBody
    where
        builtUrl = https url /: pack ("bot" ++ token) /: method
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
        is2XX x = x `mod` 200

