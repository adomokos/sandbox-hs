module Errors.ErrorReportingSpec where

import Control.Exception (evaluate)
import Errors.ErrorReporting
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Various ways to handle errors" $ do
    it "can throw an error" $ do
      myDiv1 4 2 `shouldBe` 2.0
      evaluate(myDiv1 2 0) `shouldThrow` errorCall "Division by zero"

    it "can use Maybe" $ do
      myDiv2 4 2 `shouldBe` Just 2.0
      myDiv2 4 0 `shouldBe` Nothing

    it "can use Either" $ do
      myDiv3 4 2 `shouldBe` Right 2.0
      myDiv3 4 0 `shouldBe` Left "Division by zero"
      divSum3 4 2 1 `shouldBe` Right 6.0
      divSum3 4 0 1 `shouldBe` Left "Division by zero"

    it "can use Monad and fail to generalize it" $ do
      myDiv4 4 2 `shouldBe` Just 2.0
      myDiv4 4 0 `shouldBe` Nothing
      (myDiv4 4 2 :: Either String Float) `shouldBe` Right 2.0
      -- ((myDiv4 4 0) :: Either String Float) `shouldBe`Left "Division by zero"
