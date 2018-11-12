module Errors.ErrorReportingSpec where

import           Test.Hspec
import           Errors.ErrorReporting
import qualified Control.Exception as E
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Various ways to handle errors" $ do
    it "can throw an error" $ do
      myDiv1 4 2 `shouldBe` 2.0
      evaluate(myDiv1 2 0) `shouldThrow` errorCall "Division by zero"
