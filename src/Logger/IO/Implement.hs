module Logger.IO.Implement
  ( parseConfig,
    Config (..),
    withHandle,
  )
where

import Config
import Control.Monad (when)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool (bool)
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Text.IO as T (hPutStrLn, putStrLn)
import qualified Handlers.Logger as L
import System.Directory (doesFileExist, renameFile)
import System.FilePath (takeExtension)
import qualified System.IO as SIO
import Prelude hiding (error, log)

data Config = Config
  { cFilePath :: Maybe FilePath,
    cToConsole :: Bool,
    cLogLevel :: L.LogLevel
  }

parseConfig :: AppConfig -> Config
parseConfig AppConfig {..} =
  Config
    { cFilePath = appConfigLogger & loggerOutputFile,
      cToConsole = appConfigLogger & loggerToConsole,
      cLogLevel = appConfigLogger & loggerLogLevel
    }

-- | Handle takes maybe file to write to, whether to write to console or not and loglevel
withHandle :: (MonadIO m, MonadMask m) => Config -> (L.Handle m -> m a) -> m a
withHandle c f = case c & cFilePath of
  Nothing -> f $ L.Handle (L.Config $ c & cLogLevel) (\l t -> liftIO $ when (c & cToConsole) (printMsg l t))
  Just fp -> do
    logFp <- case takeExtension fp of
      ".log" -> return fp
      _ -> (liftIO . doesFileExist $ fp) >>= bool (return ()) (liftIO $ renameFile fp (fp <> ".log")) >> return (fp <> ".log")
    bracket
      (liftIO $ SIO.openFile logFp SIO.AppendMode)
      (liftIO . SIO.hClose)
      ( \h ->
          f $
            L.Handle
              (L.Config $ c & cLogLevel)
              ( \l t -> liftIO $ do
                  when (c & cToConsole) (printMsg l t)
                  SIO.hIsWritable h >>= flip when (writeFileMsg l t h)
              )
      )
  where
    msg l t = (pack . show $ l) <> ": " <> t
    printMsg :: L.LogLevel -> Text -> IO ()
    printMsg l t = T.putStrLn $ msg l t
    writeFileMsg :: L.LogLevel -> Text -> SIO.Handle -> IO ()
    writeFileMsg l t h = T.hPutStrLn h $ msg l t
