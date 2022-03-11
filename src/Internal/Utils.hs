module Internal.Utils where
import           Control.Concurrent     (threadDelay)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Char              (toLower)
import           Data.Text              (Text, unpack)
import           Exceptions.Request.Web
import           Handlers.Bot           (Command (Command),
                                         CommandType (Help, Repeat, Start))
import qualified Handlers.Logger        as L
import           System.Exit            (exitFailure)

commandFromString :: String -> usInf -> Maybe (Command usInf)
commandFromString c ui = case map toLower c of
    "/repeat" -> Just $ Command ui Repeat
    "/help"   -> Just $ Command ui Help
    "/start"  -> Just $ Command ui Start
    _         -> Nothing

handleWeb :: (MonadIO m, MonadCatch m) => L.Handle m -> Text -> a -> m a -> m a
handleWeb hL descr def =
    handle $ \e -> case fromException e of
    Just (CodeMessageException code _) -> do
        if code == 429 then do
            L.error hL (unpack $ "Error occured while " <> descr)
            L.error hL "To many requests, 25 seconds delay"
            liftIO $ threadDelay 25000
            return def
        else throwM . toException $ e
    Just (ConnectionException t) -> do
        L.error hL (unpack $ "Error occured while " <> descr)
        L.error hL $ "Connection failure: " <> unpack t <> "\n 25 seconds delay"
        liftIO $ threadDelay 25000
        return def
    Just (InvalidUrlException url mes) -> do
        L.error hL (unpack $ "Error occured while " <> descr)
        L.error hL $ "Invalid url: " <> unpack url <> "\n message: " <> unpack mes
        liftIO exitFailure
    Nothing -> throwM e

