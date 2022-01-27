module Req.Internal where
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import           Internal.Types         (Token)
import           Network.HTTP.Client    (ManagerSettings)
import           Network.HTTP.Req       (GET (GET), LbsResponse,
                                         NoReqBody (NoReqBody), QueryParam,
                                         defaultHttpConfig, https, lbsResponse,
                                         req, runReq, (/:), (=:))

-- | Function to make requests using Network.HTTP.Req library
--
-- | Can throw Exceptions from Network.HTTP.Client
makeRequestReq :: (MonadThrow m, MonadIO m) =>
    Maybe ManagerSettings ->
    Text -> -- | base api url
    Token ->
    Text -> -- | method
    [(Text, Text)] -> -- | method params
    m LbsResponse
makeRequestReq _ url token method params = do
    runReq defaultHttpConfig (req
        GET
        builtUrl
        NoReqBody
        lbsResponse
        queryParams)
    where
        builtUrl = https url /: pack ("bot" ++ token) /: method
        queryParams :: (QueryParam p, Monoid p) => p
        queryParams = mconcat $ fmap (uncurry (=:)) params
        is2XX x = x `mod` 200

