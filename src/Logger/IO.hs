module Logger.IO
    (module X)
where

import           Handlers.Logger     as X (Handle)
import           Logger.IO.Implement as X (Config (..), withHandle)
