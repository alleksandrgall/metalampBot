module Logger.IO.Implement    (  Config (..)
     , withHandle
     ) where



import           Control.Monad              (when)
import           Control.Monad.Catch        (MonadMask, bracket)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as B (hPutStrLn, putStrLn)
import           Data.Function              ((&))
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  (pack)
import           Data.Text.IO               as T (appendFile, hPutStrLn,
                                                  putStrLn)
import qualified Handlers.Logger            as L
import           Prelude                    hiding (error, log)
import qualified System.IO                  as SIO

data Config = Config {
      cFilePath  :: Maybe FilePath
    , cToConsole :: Bool
    , cLogLevel  :: L.LogLevel
}

withHandle :: (MonadIO m, MonadMask m) => Config -> (L.Handle m -> m a) -> m a
withHandle c f = do
    if isJust $ c & cFilePath then
        bracket
            (liftIO $ SIO.openFile (fromJust $ c & cFilePath) SIO.WriteMode)
            (liftIO . SIO.hClose)
            (\h -> f $ L.Handle
                (L.Config $ c & cLogLevel)
                (\l t -> liftIO $ do
                    when (c & cToConsole) (printMsg l t)
                    SIO.hIsWritable h >>= flip when (writeFileMsg l t h)))

    else f $ L.Handle (L.Config $ c & cLogLevel) (\l t -> liftIO $ when (c & cToConsole) (printMsg l t))
    where
        msg l t = (pack . show $ l) <> ": " <> t
        printMsg :: L.LogLevel -> L.MessageType -> IO ()
        printMsg l (L.JustText t) = T.putStrLn $ msg l t
        printMsg l (L.WithBs t bs) = do
             T.putStrLn $ msg l t
             B.putStrLn bs
        writeFileMsg :: L.LogLevel -> L.MessageType -> SIO.Handle -> IO ()
        writeFileMsg l (L.JustText t) h = T.hPutStrLn h $ msg l t
        writeFileMsg l (L.WithBs t bs) h = do
            T.hPutStrLn h $ msg l t
            B.hPutStrLn h bs
