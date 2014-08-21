{- |
Module      :  <Active.Alarm.GrabberSpec>
Description :  <Spec for Grabber>
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>

Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable 

<Spec Function to test the des and host pair matching function is working correctly>
-}
module Active.Alarm.GrabberSpec (main, spec) where

import Test.Hspec
import Active.Alarm.Grabber

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "MatchKeys" $ do
    it "Create a list of des and host pair that shouldbe exact." $ do
      matchKeys ([1..13] :: [Int]) [1] `shouldBe` (\xs ys -> [(x, y) | x <- xs, y <-ys]) ([1..13]::[Int]) [1]
    it "Create a list of des and host pair that should be exactly these pairs." $ do
      matchKeys [1..5] [1..2] `shouldBe` [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2),(4,1),(4,2),(5,1),(5,2)]
    it "Create a list of des and host pair that shouldbe the same size of the product of list 1 size and list 2 size." $ do
      length (matchKeys ([1..10] :: [Int]) [1..4]) `shouldBe` ((length [1..10]) * (length [1..4]))
