
module Req.Web where

import qualified Data.ByteString.Lazy as B
import           Data.Text
import qualified Handlers.Web         as Web
import           Internal.Types
import qualified Logger               as L
import           Network.HTTP.Req

data Config = Config {
    cToken :: Token
  , cUrl   :: Text
}

instance Web.Result LbsResponse where


withHandle :: (MonadHttp m) => Config -> L.Handle m -> (Web.Handle LbsResponse m -> m a) -> m a
withHandle Config{..} hL f = do
  f $ Web.Handle
    (Web.Config Nothing cToken cUrl)
    hL
    undefined


