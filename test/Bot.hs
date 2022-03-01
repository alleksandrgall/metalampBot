{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bot where

import           Control.Monad                (Monad (..), mapM_, when)
import           Control.Monad.RWS            (MonadState (get, put),
                                               MonadWriter (tell),
                                               Monoid (mempty), RWS, asks,
                                               modify, runRWS, (<>))
import           Data.Bifunctor               (Bifunctor (first, second))
import           Data.Function                ((&))
import           Data.HashMap.Internal.Strict (HashMap, insert, lookup)
import           Data.Hashable                (Hashable)
import           Data.Int                     (Int64)
import           Data.String                  (IsString (..), String)
import           Data.Text                    (Text, pack, unpack)
import           GHC.Exts                     (IsList (fromList))
import           GHC.Generics                 (Generic)
import qualified Handlers.Bot                 as B
import qualified Handlers.Logger              as L
import           Prelude                      hiding (lookup)
import           Test.Hspec                   (context, describe, hspec, it,
                                               shouldSatisfy)
import           Test.QuickCheck              (Testable (property), elements,
                                               forAll)

data TestUserInfo = TestUserInfo {
    uId    :: Int
  , chatId :: Int
} deriving (Show, Eq, Generic, Ord)
instance Hashable TestUserInfo

data Gettable = GText String | GSticker Int64
    deriving (Show, Eq)

instance IsString Gettable where
  fromString s = GText s

data ServerContent = CBAnswer {
    cbAnswerMes :: Text
  , cbAnswerId  :: B.CallbackQuery TestUserInfo
  } |
  Message {
      mesUi      :: TestUserInfo
    , mesContent :: B.SendContent Gettable
  } deriving (Show, Eq)

type Server = [ServerContent]

type TestEnv = RWS [B.Update Gettable TestUserInfo] Server (Int64, HashMap TestUserInfo Int)

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

mockHandle :: B.Handle Gettable TestUserInfo TestEnv
mockHandle = B.Handle {
          hConfig             = mockConfig
        , hLogger             = mockLogger
        , hInit               = return ()
        , hGetUpdates         = \offset -> asks (\upds -> if null upds then (Nothing, upds) else (Just 1, upds))
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
      let allowedCallBacks  = map (\s -> B.CallbackQuery (TestUserInfo 0 0) (s <> "id") s) allowedCallbackData
          send              = map (CBAnswer (mockHandle & B.hConfig & B.cRepeatMes)) allowedCallBacks
      run (mapM_ (B.processCallback mockHandle) allowedCallBacks) `shouldSatisfy` (\(_, _, server) -> server == send)

    it "changes user's repeat number according to the allowed callback data" $ do
      property $ \targetUser ->
        forAll (elements allowedCallbackData) $ \changeToThat ->
        let toBeChangedRepeatNum = 4
            otherRepNums = [1, 2, 3, 5]
            otherUsersInfo = [uncurry TestUserInfo (x, x) | x <- [1 .. 4]]
            targetUserInfo = uncurry TestUserInfo targetUser
            userRepeats    = fromList $ (targetUserInfo, toBeChangedRepeatNum) : zip otherUsersInfo otherRepNums
            callBack       = B.CallbackQuery targetUserInfo "cbId" changeToThat
            (_, (_, newUserRepeats), _) = run $ put (0, userRepeats) >> B.processCallback mockHandle callBack
            (_, (_, shouldBeUserRepeats), _) = run $ put (0, userRepeats) >>
              B.hInsertUserRepeat mockHandle targetUserInfo (read changeToThat :: Int)
        in newUserRepeats == shouldBeUserRepeats

    it "ignores bad callback data" $ do
      property $ \targetUser changeToThat->
        let toBeChangedRepeatNum = 4
            otherRepNums = [1, 2, 3, 5]
            otherUsersInfo = [uncurry TestUserInfo (x, x) | x <- [1 .. 4]]
            targetUserInfo = uncurry TestUserInfo targetUser
            userRepeats    = fromList $ (targetUserInfo, toBeChangedRepeatNum) : zip otherUsersInfo otherRepNums
            callBack       = B.CallbackQuery targetUserInfo "cbId" (show changeToThat)
            (_, (_, newUserRepeats), server) = run $
              put (0, userRepeats) >>
              when (changeToThat `notElem` allowedCallbackData) (B.processCallback mockHandle callBack)
        in newUserRepeats == userRepeats && null server

processCommand :: IO ()
processCommand = hspec $ do
  describe "Handlers.Bot.processCommand" $ do
    let run m = runRWS m [] (0, mempty)

    it "on start sends start message (cStartMes)" $ do
      property $ \targetUser ->
        let targetUserInfo = uncurry TestUserInfo targetUser
            (_, _, send) = run $ B.processCommand mockHandle (B.Command (uncurry TestUserInfo targetUser) B.Start)
            shouldHaveBeenSend = [Message targetUserInfo (B.CGettable . GText . unpack $ mockHandle & B.hConfig & B.cStartMes)]
        in send == shouldHaveBeenSend

    it "on help sends help message (cHelpMes)" $ do
      property $ \targetUser ->
        let targetUserInfo = uncurry TestUserInfo targetUser
            (_, _, send) = run $ B.processCommand mockHandle (B.Command (uncurry TestUserInfo targetUser) B.Help)
            shouldHaveBeenSend = [Message targetUserInfo (B.CGettable . GText . unpack $ mockHandle & B.hConfig & B.cHelpMes)]
        in send == shouldHaveBeenSend

    it "on repeat sends keyboard with repeat message as it's text (cRepeatKeyboardMes)" $ do
      property $ \targetUser ->
        let targetUserInfo = uncurry TestUserInfo targetUser
            (_, _, send) = run $ B.processCommand mockHandle (B.Command (uncurry TestUserInfo targetUser) B.Repeat)
            keyboardMessage = mockHandle & B.hConfig & B.cRepeatKeyboardMes
            keyboardInfo    = [pack . show $ x | x <- [1 .. 5]]
            shouldHaveBeenSend = [Message targetUserInfo (B.CKeyboard keyboardMessage (B.Keyboard keyboardInfo))]
        in send == shouldHaveBeenSend

processMessage :: IO ()
processMessage = hspec $ do
  describe "Handlers.Bot.processMessage" $ do
    let messageTarget1 = B.MessageGet targetUser1 "Hello"
        messageTarget2 = B.MessageGet targetUser2 (GSticker 123456)
        targetUser1 = TestUserInfo 5 6
        targetUser2 = TestUserInfo 2 3

    it "sends message to the user it came from as many times as the number of repits of the user" $ do
      let userRepeats = fromList [(targetUser2, 2), (targetUser1, 3)]
          run m = runRWS m [] (0, userRepeats)
          sending = B.processMessage mockHandle messageTarget1 >> B.processMessage mockHandle messageTarget2
          shouldBeSend = replicate 3 (Message targetUser1 "Hello") ++
            replicate 2 (Message targetUser2 (B.CGettable . GSticker $ 123456))
      run sending `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)

    it "if user's number of repeats has not been set sends message as many times as the default number of repeats (cBaseRepeat)" $ do
      let run m = runRWS m [] (0, mempty)
          mockHandle' = mockHandle {B.hConfig = (mockHandle & B.hConfig) {B.cBaseRepeat = 4}, B.hGetUserRepeat = \_ -> return Nothing}
          sending = B.processMessage mockHandle' messageTarget1
          shouldBeSend = replicate 4 (Message targetUser1 "Hello")
      run sending `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)

processUpdates :: IO ()
processUpdates = hspec $ do
  describe "Handlers.Bot.processUpdates" $ do
    let cbExample = B.UCallbackQuary (B.CallbackQuery (TestUserInfo 1 1) "cbId" "1")
        mesExample = B.UMessage (B.MessageGet (TestUserInfo 2 2) "message")
        comExample = B.UCommand (B.Command (TestUserInfo 3 3) B.Start)

    context "On receiving some updates" $
      it "processes update's content" $ do
        let run m = runRWS m [mesExample, cbExample, comExample] (0, mempty)
            shouldBeSend = replicate (mockHandle & B.hConfig & B.cBaseRepeat) (Message (TestUserInfo 2 2) "message") ++
              [CBAnswer (mockHandle & B.hConfig & B.cRepeatMes) (B.CallbackQuery (TestUserInfo 1 1) "cbId" "1")] ++
              [Message (TestUserInfo 3 3) (B.CGettable . fromString . unpack $ mockHandle & B.hConfig & B.cStartMes)]
        run ((mockHandle & B.hGetOffset) >>= (mockHandle & B.hGetUpdates) >>= uncurry (B.processUpdates mockHandle))
          `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)

    context "On receiving no updates" $
      it "does nothing" $ do
        let run m = runRWS m [] (0, mempty)
            shouldBeSend = []
        run ((mockHandle & B.hGetOffset) >>= (mockHandle & B.hGetUpdates) >>= uncurry (B.processUpdates mockHandle))
          `shouldSatisfy` (\(_, _, send) -> send == shouldBeSend)
