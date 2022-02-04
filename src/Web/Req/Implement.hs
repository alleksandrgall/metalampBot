
module Web.Req.Implement
  (Config(..)
  ,withHandle)
where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             (ToJSON)
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import qualified Handlers.Logger        as L
import qualified Handlers.Web           as Web
import           Internal.Types         (Protocol, Token)
import           Network.HTTP.Req       (LbsResponse, responseBody,
                                         responseStatusCode,
                                         responseStatusMessage)
import           Web.Req.Internal       (sendRequestReq)


data Config = Config {
    cUrl      :: Text
  , cProtocol :: Protocol
}

withHandle :: (MonadIO m, MonadThrow m, ToJSON b) => Config -> L.Handle m -> (Web.Handle m LbsResponse b -> m a) -> m a
withHandle Config{..} hL f =
  f $ Web.Handle
    (Web.Config Nothing cUrl cProtocol)
    hL
    sendRequestReq
