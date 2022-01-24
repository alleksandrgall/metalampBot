
module Req.Web where

import qualified Data.ByteString.Lazy as B
import           Data.Text
import qualified Handlers.Web         as Web
import qualified Logger               as L
import           Network.HTTP.Req

data Config = Config {
    cToken :: Web.Token
  , cUrl   :: Text
}

withHandle :: (MonadHttp m) => Config -> L.Handle m -> (Web.Handle m -> m a) -> m a
withHandle Config{..} hL f = do
  f $ Web.Handle
    (Web.Config Nothing cToken cUrl)
    hL
    undefined


