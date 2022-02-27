module Internal.Utils where
import           Data.Char      (toLower)
import           Data.Semigroup (Max (Max, getMax), Option (Option, getOption))
import           Handlers.Bot

commandFromString :: String -> UserInfo -> Maybe Command
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = fmap getMax . getOption . foldMap (Option . Just . Max)
