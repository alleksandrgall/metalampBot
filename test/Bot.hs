{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           Data.Text                    (Text)
import           GHC.Exts                     (IsString (fromString))
import qualified Handlers.Bot                 as B
import qualified Handlers.Logger              as L
import           Prelude                      hiding (lookup)
import           Test.Hspec                   (describe, hspec, it, shouldBe,
                                               shouldSatisfy)
import           Test.QuickCheck              (Testable (property), elements,
                                               forAll, listOf)

instance Hashable B.UserInfo

data Gettable = GText String | GSticker Int
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

processCallBack :: IO ()
processCallBack = hspec $ do
  describe "Testing callback quary processing" $ do
    let allowedCallBackData = ["1", "2", "3", "4", "5"]

    it "sends an answer if the callback data is allowed" $ do
       let run m             = runRWS m [] (0, mempty)
           allowedCallBacks  = map (\s -> B.CallbackQuery (B.UserInfo 0 0) (s <> "id") s) allowedCallBackData
           send              = map (CBAnswer (mockHandle & B.hConfig & B.cRepeatMes)) allowedCallBacks
       run (mapM_ (B.processCallback mockHandle) allowedCallBacks) `shouldSatisfy` (\(_, _, server) -> server == send)

    it "changes user's repeat number according to the allowed callback data" $ do
      let run m = runRWS m [] (0, mempty)
      property $ \(toChangeId, toChangeChat) toChangeRepNum otherUsers ->
        forAll (elements [1..5]) $ \changeToThat ->
        forAll (listOf $ elements [1..5]) $ \otherRepNums ->
        let userToChange   = B.UserInfo toChangeId toChangeChat
            userRepeats    = fromList $ (userToChange, toChangeRepNum) : zip (map (uncurry B.UserInfo) otherUsers) otherRepNums
            callBack       = B.CallbackQuery userToChange "cbId" (show changeToThat)
            (_, (_, newUserRepeats), _) = run $ put (0, userRepeats) >> B.processCallback mockHandle callBack
        in newUserRepeats == insert userToChange changeToThat userRepeats

    it "ignores bad callback data" $ do
      let run m = runRWS m [] (0, mempty)
      property $ \(toChangeId, toChangeChat) toChangeRepNum otherUsers changeToThat ->
        forAll (listOf $ elements [1..5]) $ \otherRepNums ->
        let userToChange   = B.UserInfo toChangeId toChangeChat
            userRepeats    = fromList $ (userToChange, toChangeRepNum) : zip (map (uncurry B.UserInfo) otherUsers) otherRepNums
            callBack       = B.CallbackQuery userToChange "cbId" (show changeToThat)
            (_, (_, newUserRepeats), server) = run $
              put (0, userRepeats) >>
              when (changeToThat `notElem` allowedCallBackData) (B.processCallback mockHandle callBack)
        in newUserRepeats == userRepeats && null server
