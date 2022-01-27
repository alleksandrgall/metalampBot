{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Bot.Telegram.Internal.ResponseTypes
import           Config
import           Control.Concurrent.STM              (TVar, newTVarIO)
import           Control.Monad.Catch
import qualified Data.ByteString.Char8               as B
import           Data.Function                       ((&))
import           Data.Int                            (Int64)
import           Data.Map                            as M
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T
import qualified Logger.IO                           as L
import qualified Handlers.Logger                     as L
import           Network.HTTP.Client                 (HttpException (HttpExceptionRequest, InvalidUrlException),
                                                      HttpExceptionContent (..),
                                                      Response (responseStatus))
import           Network.HTTP.Req
import           Network.HTTP.Types.Status


type NumberOfRepeats = Map Int64 Int

data Config = Config {
    cHelpMessage      :: T.Text
  , cDefaultRepeatNum :: Int
  , cToken            :: String
}

data Handle = Handle {
      hConfig                   :: String
    , hLogger                   :: L.Handle IO
    , hOffset                   :: TVar Int
    , hNumberOfRepeats          :: TVar NumberOfRepeats
    , hInit                     :: IO ()
    , hGetUpdates               :: IO [Update]
    , hSendMessage              :: Message -> Int64 -> IO ()
    , hSendRepeatNumberKeyboard :: Int64 -> IO ()
    , hAnswerCallBack           :: CallbackQuery -> Int64 -> IO ()
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
  let builtUrl = https "api.telegram.org" /: pack ("bot" ++ (appConfig & appConfigToken)) /: "getUpdates"
      queryParams :: (QueryParam p, Monoid p) => p
      queryParams = mconcat $ fmap (uncurry (=:)) ([] :: [(Text, Text)])
  resp <- catch (responseBody <$> runReq defaultHttpConfig (req
      GET
      builtUrl
      NoReqBody
      bsResponse
      queryParams))
      (\case
        VanillaHttpException (HttpExceptionRequest rq eContent) ->
          case eContent of
            StatusCodeException rs bRs ->
              do
              putStrLn "Code"
              print $ rs & responseStatus & statusCode
              putStrLn "Message"
              print $ rs & responseStatus & statusMessage
              return bRs  -- server answered with anything but 2XX
            other                    -> undefined
        VanillaHttpException (InvalidUrlException url er) -> undefined
        (JsonHttpException js) -> undefined
        )
  print "Body"
  B.putStrLn resp
  return ()
