module Internal.Utils where
import           Data.Char    (toLower)
import           Handlers.Bot

commandFromString :: String -> UserInfo -> Maybe Command
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing
