module Bot.Telegram (module X) where

import           Bot.Telegram.Implement as X (Config (..), parseConfig,
                                              withHandle)
import           Handlers.Bot           as X (Handle)
