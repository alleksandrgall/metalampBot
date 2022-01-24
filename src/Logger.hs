{-# LANGUAGE OverloadedStrings #-}
module Logger
    (  Logger.Config (..)
     , MessageType
     , log
     , Handle
     , withHandle
     , LogLevel
     , debug
     , info
     , warning
     , error
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
import           Handlers.Logger            as L
import           Prelude                    hiding (error, log)
import qualified System.IO                  as SIO

data Config = Config {
      cFilePath  :: Maybe FilePath
    , cToConsole :: Bool
    , cLogLevel  :: L.LogLevel
}

withHandle :: (MonadIO m, MonadMask m) => Logger.Config -> (L.Handle m -> m a) -> m a
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
        printMsg :: LogLevel -> MessageType -> IO ()
        printMsg l (JustText t) = T.putStrLn $ msg l t
        printMsg l (WithBs t bs) = do
             T.putStrLn $ msg l t
             B.putStrLn bs
        writeFileMsg :: LogLevel -> MessageType -> SIO.Handle -> IO ()
        writeFileMsg l (JustText t) h = T.hPutStrLn h $ msg l t
        writeFileMsg l (WithBs t bs) h = do
            T.hPutStrLn h $ msg l t
            B.hPutStrLn h bs
