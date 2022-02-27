module Internal.Utils where
import           Data.Char      (toLower)
import           Data.Semigroup (Max (Max, getMax), Option (Option, getOption))
import           Handlers.Bot

commandFromString :: String -> usInf -> Maybe (Command usInf)
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing
