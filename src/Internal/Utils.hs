module Internal.Utils where
import           Data.Char    (toLower)
import           Handlers.Bot (Command (Command),
                               CommandType (Help, Repeat, Start))

commandFromString :: String -> usInf -> Maybe (Command usInf)
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing
