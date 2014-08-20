module Active.Alarm.GrabberSpec (main, spec) where

import Test.Hspec
import Active.Alarm.Grabber

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False
