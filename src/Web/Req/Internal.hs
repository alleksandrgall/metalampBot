module Web.Req.Internal where
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (ToJSON)
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import           Internal.Types         (Protocol (..), Token)
import           Network.HTTP.Client    (ManagerSettings)
import           Network.HTTP.Req       (GET (GET), LbsResponse,
                                         NoReqBody (NoReqBody), POST (POST),
                                         QueryParam, ReqBodyJson (ReqBodyJson),
                                         ReqBodyLbs (ReqBodyLbs),
                                         defaultHttpConfig, http, https,
                                         lbsResponse, req, runReq, (/:), (=:))

-- | Function to make requests using Network.HTTP.Req library
--
-- | Can throw Exceptions from Network.HTTP.Client
sendRequestReq :: (MonadThrow m, MonadIO m, ToJSON b) =>
    Maybe ManagerSettings ->
    Maybe b -> -- | possible request body
    Protocol ->
    Text -> -- | base api url
    Text -> -- | method
    [(Text, Text)] -> -- | method params
    m LbsResponse
sendRequestReq _ maybeBody prot url method params =
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
        builtHttp = http  url /: method
        builtHttps = https url /: method
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
