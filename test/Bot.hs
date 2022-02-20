{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Bot where

import           Control.Monad                (when)
import           Control.Monad.RWS            (MonadState (get, put), RWS,
                                               runRWS)
import           Control.Monad.Reader         (asks)
import           Control.Monad.State          (modify)
import           Control.Monad.Writer         (MonadWriter (tell))
import           Data.Bifunctor               (Bifunctor (first, second))
import           Data.Function                ((&))
import           Data.HashMap.Internal.Strict (HashMap, fromList, insert,
                                               lookup)
import           Data.Hashable                (Hashable)
import           Data.Int                     (Int64)
import           Data.String                  (IsString)
import           Data.Text                    (Text, pack, unpack)
import           GHC.Exts                     (IsString (fromString))
import qualified Handlers.Bot                 as B
import qualified Handlers.Logger              as L
import           Prelude                      hiding (lookup)
import           Test.Hspec                   (describe, hspec, it, shouldBe,
                                               shouldSatisfy)
import           Test.QuickCheck              (Args (maxSize, maxSuccess),
                                               Testable (property), elements,
                                               forAll, listOf, quickCheck,
                                               quickCheckWith, stdArgs)

instance Hashable B.UserInfo

data Gettable = GText String | GSticker Int64
    deriving (Show, Eq)

instance IsString Gettable where
  fromString s = GText s

data ServerContent = CBAnswer {
    cbAnswerMes :: Text
  , cbAnswerId  :: B.CallbackQuery
  } |
  Message {
      mesUi      :: B.UserInfo
    , mesContent :: B.SendContent Gettable
  } deriving (Show, Eq)

type Server = [ServerContent]

type TestEnv = RWS [B.Update Gettable] Server (Int64, HashMap B.UserInfo Int)

mockConfig = B.Config {
    cBaseRepeat        = 1
  , cStartMes          = "Start"
  , cHelpMes           = "Help"
  , cRepeatMes         = "Repeat"
  , cRepeatKeyboardMes = "Repeat Keyboard"
}

mockLogger ::(Monad m) => L.Handle m
mockLogger = L.Handle {
    L.hConfig = L.Config L.Info
  , L.hLogMessage = \_ _ -> return ()
}

mockHandle :: B.Handle Gettable TestEnv
mockHandle = B.Handle {
          hConfig             = mockConfig
        , hLogger             = mockLogger
        , hInit               = return ()
        , hSleep              = \_ -> return ()
        , hGetUpdates         = \offset -> asks (filter (\B.Update {..} -> uId >= offset))
        , hSendMes            = \B.MessageSend {..} -> tell [Message msUserInfo msContent]
        , hAnswerCallback     = \t cb               -> tell [CBAnswer t cb]
        , hGetOffset          = fst <$> get
        , hSetOffset          = modify . first . const
        , hInsertUserRepeat   = \ui r -> modify (second (insert ui r))
        , hGetUserRepeat      = \ui   -> lookup ui . snd <$> get
}

processCallback :: IO ()
processCallback = hspec $ do
  describe "Handlers.Bot.processCallback" $ do
    let allowedCallbackData = ["1", "2", "3", "4", "5"]
        run m               = runRWS m [] (0, mempty)

    it "sends an answer if the callback data is allowed" $ do
      let allowedCallBacks  = map (\s -> B.CallbackQuery (B.UserInfo 0 0) (s <> "id") s) allowedCallbackData
          send              = map (CBAnswer (mockHandle & B.hConfig & B.cRepeatMes)) allowedCallBacks
      run (mapM_ (B.processCallback mockHandle) allowedCallBacks) `shouldSatisfy` (\(_, _, server) -> server == send)

    it "changes user's repeat number according to the allowed callback data" $ do
      property $ \targetUser toBeChangedRepeatNum otherRepNums otherUsers ->
        forAll (elements allowedCallbackData) $ \changeToThat ->
        let targetUserInfo = uncurry B.UserInfo targetUser
            userRepeats    = fromList $ (targetUserInfo, toBeChangedRepeatNum) : zip (map (uncurry B.UserInfo) otherUsers) otherRepNums
            callBack       = B.CallbackQuery targetUserInfo "cbId" changeToThat
            (_, (_, newUserRepeats), _) = run $ put (0, userRepeats) >> B.processCallback mockHandle callBack
            (_, (_, shouldBeUserRepeats), _) = run $ put (0, userRepeats) >>
              B.hInsertUserRepeat mockHandle targetUserInfo (read changeToThat :: Int)
        in newUserRepeats == shouldBeUserRepeats

    it "ignores bad callback data" $ do
      property $ \targetUser toBeChangedRepeatNum otherUsers otherRepNums changeToThat ->
        let targetUserInfo = uncurry B.UserInfo targetUser
            userRepeats    = fromList $ (targetUserInfo, toBeChangedRepeatNum) : zip (map (uncurry B.UserInfo) otherUsers) otherRepNums
            callBack       = B.CallbackQuery targetUserInfo "cbId" (show changeToThat)
            (_, (_, newUserRepeats), server) = run $
              put (0, userRepeats) >>
              when (changeToThat `notElem` allowedCallbackData) (B.processCallback mockHandle callBack)
        in newUserRepeats == userRepeats && null server

processCommand :: IO ()
processCommand = hspec $ do
  describe "Handlers.Bot.processCommand" $ do
    it "on start sends start message (cStartMes)" $ do
      property $ \targetUser ->
        let run m = runRWS m [] (0, mempty)
            targetUserInfo = uncurry B.UserInfo targetUser
            (_, _, send) = run $ B.processCommand mockHandle (B.Command (uncurry B.UserInfo targetUser) B.Start)
            shouldHaveBeenSend = [Message targetUserInfo (B.CGettable . GText . unpack $ mockHandle & B.hConfig & B.cStartMes)]
        in send == shouldHaveBeenSend
    it "on help sends help message (cHelpMes)" $ do
      property $ \targetUser ->
        let run m = runRWS m [] (0, mempty)
            targetUserInfo = uncurry B.UserInfo targetUser
            (_, _, send) = run $ B.processCommand mockHandle (B.Command (uncurry B.UserInfo targetUser) B.Help)
            shouldHaveBeenSend = [Message targetUserInfo (B.CGettable . GText . unpack $ mockHandle & B.hConfig & B.cHelpMes)]
        in send == shouldHaveBeenSend
    it "on repeat sends keyboard and repeat message (cRepeatKeyboardMes)" $ do
      property $ \targetUser ->
        let run m = runRWS m [] (0, mempty)
            targetUserInfo = uncurry B.UserInfo targetUser
            (_, _, send) = run $ B.processCommand mockHandle (B.Command (uncurry B.UserInfo targetUser) B.Repeat)
            keyboardMessage = mockHandle & B.hConfig & B.cRepeatKeyboardMes
            keyboardInfo    = [("1", "1"), ("2", "2"), ("3" , "3"), ("4", "4"), ("5", "5")]
            shouldHaveBeenSend = [Message targetUserInfo (B.CKeyboard keyboardMessage (B.Keyboard keyboardInfo))]
        in send == shouldHaveBeenSend

processMessage :: IO ()
processMessage = hspec $ do
  describe "Handlers.Bot.processMessage" $ do
    let messageTarget1 = B.MessageGet targetUser1 0 (GText "Hello")
        messageTarget2 = B.MessageGet targetUser2 0 (GSticker 123456)
        targetUser1 = B.UserInfo 5 6
        targetUser2 = B.UserInfo 2 3
    it "sends message to the user it came from as many times as the number of repits of the user" $ do
      let userRepeats = fromList [(targetUser2, 2), (targetUser1, 3)]
          run m = runRWS m [] (0, userRepeats)
          sending = B.processMessage mockHandle messageTarget1 >> B.processMessage mockHandle messageTarget2
          shouldBeSend = replicate 3 (Message targetUser1 (B.CGettable . GText $ "Hello")) ++
            replicate 2 (Message targetUser2 (B.CGettable . GSticker $ 123456))
      run sending `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)
    it "if user's number of repeats is not benn set sends message as many times as the default number of repeats (cBaseRepeat)" $ do
      let run m = runRWS m [] (0, mempty)
          mockHandle' = mockHandle {B.hConfig = (mockHandle & B.hConfig) {B.cBaseRepeat = 4}, B.hGetUserRepeat = \_ -> return Nothing}
          sending = B.processMessage mockHandle' messageTarget1
          shouldBeSend = replicate 4 (Message targetUser1 (B.CGettable . GText $ "Hello"))
      run sending `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)

processUpdates :: IO ()
processUpdates = hspec $ do
  describe "Handlers.Bot.processUpdates" $ do
    let cbExample = B.UCCallbackQuary (B.CallbackQuery (B.UserInfo 1 1) "cbId" "1")
        mesExample = B.UCMessage (B.MessageGet (B.UserInfo 2 2) 0 "message")
        comExample = B.UCCommand (B.Command (B.UserInfo 3 3) B.Start)
    it "updates offset to be the highest update id of the updates being processed plus 1" $ do
      let run m = runRWS m [B.Update 14 mesExample, B.Update 2 mesExample, B.Update 17 mesExample, B.Update 5 mesExample] (0, mempty)
      run ((mockHandle & B.hGetOffset) >>= (mockHandle & B.hGetUpdates) >>= B.processUpdates mockHandle)
        `shouldSatisfy` (\(_, (offset, _), _) -> offset == 18)

    it "processing update's content" $ do
      let run m = runRWS m [B.Update 14 mesExample, B.Update 2 cbExample, B.Update 17 comExample] (0, mempty)
          shouldBeSend = replicate (mockHandle & B.hConfig & B.cBaseRepeat) (Message (B.UserInfo 2 2) (B.CGettable "message")) ++
            [CBAnswer (mockHandle & B.hConfig & B.cRepeatMes) (B.CallbackQuery (B.UserInfo 1 1) "cbId" "1")] ++
            [Message (B.UserInfo 3 3) (B.CGettable . fromString . unpack $ mockHandle & B.hConfig & B.cStartMes)]
      run ((mockHandle & B.hGetOffset) >>= (mockHandle & B.hGetUpdates) >>= B.processUpdates mockHandle)
        `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)
