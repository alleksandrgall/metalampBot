module Logger.IO
    (module X)
where

import           Handlers.Logger     as X (Handle, LogLevel (..))
import           Logger.IO.Implement as X (Config (..), parseConfig, withHandle)
