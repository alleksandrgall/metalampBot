{-# LANGUAGE OverloadedStrings #-}
module Logger 
    (  Logger.Config (..)
     , log
     , Handle
     , withHandle
     , LogLevel
     , debug
     , info
     , warning
     , error
     ) where


import Handlers.Logger as L
import Data.Text.IO as T (putStrLn, appendFile, hPutStr)
import Data.Text (append, pack)
import Data.Function ((&))
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import System.IO hiding (Handle)
import Prelude hiding (log, error)
import Control.Exception (bracket)

data Config = Config {
      cFilePath :: Maybe FilePath
    , cToConsole :: Bool
    , cLogLevel :: L.LogLevel
}

withHandle :: Logger.Config -> (L.Handle IO -> IO a) -> IO a
withHandle c f = do
    if isJust $ c & cFilePath then
        withFile (fromJust $ c & cFilePath) WriteMode
            (\h -> do
                let logHandle = L.Handle
                        (L.Config $ c & cLogLevel)
                        (\l t -> do
                            when (c & cToConsole) (T.putStrLn $ msg l t)
                            hIsWritable h >>= flip when (T.hPutStr h $ msg l t))
                f logHandle)
    else do
        let logHandle = L.Handle
                        (L.Config $ c & cLogLevel)
                        (\l t -> do
                            when (c & cToConsole) (T.putStrLn $ msg l t))
        f logHandle
    where msg l t = (pack . show $ l) `append` ": " `append` t `append` "\n"
