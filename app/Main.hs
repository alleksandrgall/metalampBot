
module Main where
import qualified Bot.Logger as L 
import Config
import Bot.Telegram.Internal.ResponseTypes
import Data.Map as M
import Data.Int (Int64)
import qualified Data.Text as T
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Function ((&))

type NumberOfRepeats = Map Int64 Int

data Config = Config {
    cHelpMessage :: T.Text
  , cDefaultRepeatNum :: Int
  , cToken :: String
}

data Handle = Handle { 
      hConfig :: Config
    , hLogger :: L.Handle IO
    , hOffset :: TVar Int
    , hNumberOfRepeats :: TVar NumberOfRepeats
    , hInit :: IO ()
    , hGetUpdates :: IO [Update]
    , hSendMessage :: Message -> Int64 -> IO ()
    , hSendRepeatNumberKeyboard :: Int64 -> IO ()
    , hAnswerCallBack :: CallbackQuery -> Int64 -> IO ()
}

withHandle :: L.Handle IO -> AppConfig -> (Handle -> IO a) -> IO a
withHandle hLog appConfig m = do
  let myConfig = Config 
        (appConfig & appConfigHelp & helpMessage) (appConfig & appConfigRepeat & repeatDefaultNumber) (appConfig & appConfigToken)
  myOffset <- newTVarIO 0
  --myNumberOfRepeats <- newTVarIO (M.fromList []) 

  return undefined 
main = do
  appConfig <- fetchConfig
  return ()
