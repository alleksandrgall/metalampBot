{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot where

import Control.Monad (Monad (..), mapM_, when)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bifunctor (Bifunctor (first, second))
import Data.Function ((&))
import Data.HashMap.Internal.Strict (HashMap, insert, lookup)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.String (IsString (..), String)
import Data.Text (Text, pack, unpack)
import GHC.Exts (IsList (fromList))
import GHC.Generics (Generic)
import qualified Handlers.Bot as B
import qualified Handlers.Logger as L
import Test.Hspec
  ( context,
    describe,
    hspec,
    it,
    shouldSatisfy,
  )
import Test.QuickCheck
  ( Arbitrary,
    Testable (property),
    elements,
    forAll,
  )
import Prelude hiding (lookup)

type TestUserInfo = Int

type Gettable = Text

data ServerContent
  = CBAnswer
      { cbAnswerMes :: Text,
        cbAnswerId :: B.CallbackQuery TestUserInfo
      }
  | Message
      { mesUi :: TestUserInfo,
        mesContent :: B.SendContent Gettable
      }
  deriving (Show, Eq)

type Server = [ServerContent]

type TestEnv = ReaderT [B.Update Gettable TestUserInfo] (Writer Server)

mockConfig =
  B.Config
    { cBaseRepeat = 1,
      cStartMes = "Start",
      cHelpMes = "Help",
      cRepeatMes = "Repeat",
      cRepeatKeyboardMes = "Repeat Keyboard"
    }

mockLogger :: (Monad m) => L.Handle m
mockLogger =
  L.Handle
    { L.hConfig = L.Config L.Info,
      L.hLogMessage = \_ _ -> return ()
    }

mockHandle :: B.Handle Gettable TestUserInfo TestEnv
mockHandle =
  B.Handle
    { hConfig = mockConfig,
      hLogger = mockLogger,
      hInit = return 2,
      hGetUpdates = \offset -> asks (\upds -> if null upds then (Nothing, upds) else (Just 1, upds)),
      hSendMes = \B.MessageSend {..} -> tell [Message msUserInfo msContent],
      hAnswerCallback = \t cb -> tell [CBAnswer t cb]
    }

processCallback :: IO ()
processCallback = hspec $ do
  describe "Handlers.Bot.processCallback" $ do
    let allowedCallbackData = ["1", "2", "3", "4", "5"]
        run m = runWriter (runReaderT m [])

    it "sends an answer if the callback data is allowed" $ do
      property $ \targetUser -> do
        let allowedCallBacks = map (\s -> B.CallbackQuery targetUser (s <> "id") s) allowedCallbackData
            send = map (CBAnswer (mockHandle & B.hConfig & B.cRepeatMes)) allowedCallBacks
        run (mapM_ (\c -> B.processCallback mockHandle c (HM.fromList [])) allowedCallBacks) `shouldSatisfy` (\(_, server) -> server == send)

    it "returns changed user repeat hashmap according to the cb data" $ do
      property $ \userRepeatList targetUser ->
        forAll (elements allowedCallbackData) $ \changeToThat ->
          let userRepeats = HM.fromList userRepeatList
              callBack = B.CallbackQuery targetUser "cbId" changeToThat
              (newUserRepeats, _) = run $ B.processCallback mockHandle callBack userRepeats
              shouldBeUserRepeats = HM.insert targetUser (read changeToThat :: Int) userRepeats
           in newUserRepeats == shouldBeUserRepeats

    it "ignores bad callback data" $ do
      property $ \userRepeatList targetUser changeToThat ->
        let userRepeats = HM.fromList userRepeatList
            callBack = B.CallbackQuery targetUser "cbId" changeToThat
            (newUserRepeats, send) =
              run $
                if changeToThat `notElem` allowedCallbackData
                  then B.processCallback mockHandle callBack userRepeats
                  else return userRepeats
         in newUserRepeats == userRepeats && null send

processCommand :: IO ()
processCommand = hspec $ do
  describe "Handlers.Bot.processCommand" $ do
    let run m = runWriter (runReaderT m [])

    context "on start" $
      it "sends start message (cStartMes)" $ do
        property $ \targetUser ->
          let (_, send) = run $ B.processCommand mockHandle (B.Command targetUser B.Start)
              shouldHaveBeenSend = [Message targetUser (B.CGettable $ mockHandle & B.hConfig & B.cStartMes)]
           in send == shouldHaveBeenSend

    context "on help" $
      it "sends help message (cHelpMes)" $ do
        property $ \targetUser ->
          let (_, send) = run $ B.processCommand mockHandle (B.Command targetUser B.Help)
              shouldHaveBeenSend = [Message targetUser (B.CGettable $ mockHandle & B.hConfig & B.cHelpMes)]
           in send == shouldHaveBeenSend

    context "on repeat" $
      it "sends keyboard with repeat message as it's text (cRepeatKeyboardMes)" $ do
        property $ \targetUser ->
          let (_, send) = run $ B.processCommand mockHandle (B.Command targetUser B.Repeat)
              keyboardMessage = mockHandle & B.hConfig & B.cRepeatKeyboardMes
              shouldHaveBeenSend = [Message targetUser (B.CKeyboard keyboardMessage)]
           in send == shouldHaveBeenSend

processMessage :: IO ()
processMessage = hspec $ do
  describe "Handlers.Bot.processMessage" $ do
    let run m = runWriter (runReaderT m [])
    it "sends message to the user it came from as many times as the number of repeats of the user" $ do
      property $ \targetUser tuRepeat userRepeatList ->
        let userRepeats = HM.insert targetUser tuRepeat $ fromList userRepeatList
            sending = B.processMessage mockHandle (B.MessageGet targetUser "Hello") userRepeats
            shouldBeSend = replicate tuRepeat (Message targetUser (B.CGettable "Hello"))
         in run sending `shouldSatisfy` (\(_, send) -> send == shouldBeSend)

    it "if user's number of repeats has not been set sends message as many times as the default number of repeats (cBaseRepeat)" $ do
      property $ \targetUser baseRepeat userRepeatList ->
        let mockHandle' = mockHandle {B.hConfig = (mockHandle & B.hConfig) {B.cBaseRepeat = baseRepeat}}
            userRepeats = fromList $ filter (\(u, _) -> u /= targetUser) userRepeatList
            sending = B.processMessage mockHandle' (B.MessageGet targetUser "Hello") userRepeats
            shouldBeSend = replicate baseRepeat (Message targetUser "Hello")
         in run sending `shouldSatisfy` (\(_, send) -> send == shouldBeSend)

processUpdates :: IO ()
processUpdates = hspec $ do
  describe "Handlers.Bot.processUpdates" $ do
    let cbExample = B.UCallbackQuary (B.CallbackQuery 1 "cbId" "1")
        mesExample = B.UMessage (B.MessageGet 2 "message")
        comExample = B.UCommand (B.Command 3 B.Start)
    context "On receiving some updates" $
      it "processes update's content" $ do
        let run m = runWriter (runReaderT m [mesExample, cbExample, comExample])
            shouldBeSend =
              replicate (mockHandle & B.hConfig & B.cBaseRepeat) (Message 2 "message")
                ++ [CBAnswer (mockHandle & B.hConfig & B.cRepeatMes) (B.CallbackQuery 1 "cbId" "1")]
                ++ [Message 3 (B.CGettable . fromString . unpack $ mockHandle & B.hConfig & B.cStartMes)]
        run ((mockHandle & B.hGetUpdates) 0 >>= (B.processUpdates mockHandle (fromList []) . snd))
          `shouldSatisfy` (\(_, send) -> send == shouldBeSend)

    context "On receiving no updates" $
      it "does nothing" $ do
        let run m = runWriter (runReaderT m [])
            shouldBeSend = []
        run ((mockHandle & B.hGetUpdates) 0 >>= (B.processUpdates mockHandle (fromList []) . snd))
          `shouldSatisfy` (\(_, send) -> send == shouldBeSend)
