
module Web.Req.Implement
  (Config(..)
  ,withHandle)
where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import qualified Handlers.Logger        as L
import qualified Handlers.Web           as Web
import           Internal.Types         (Token)
import           Network.HTTP.Req       (LbsResponse, responseBody,
                                         responseStatusCode,
                                         responseStatusMessage)
import           Web.Req.Internal       (sendRequestReq)


data Config = Config {
    cToken :: Token
  , cUrl   :: Text
}

withHandle :: (MonadIO m, MonadThrow m) => Config -> L.Handle m -> (Web.Handle LbsResponse m -> m a) -> m a
withHandle Config{..} hL f =
  f $ Web.Handle
    (Web.Config Nothing cToken cUrl)
    hL
    sendRequestReq
