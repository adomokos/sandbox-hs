{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.Ex02_WorkingDirectlySpec where

import Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Exts (fromList)
import Test.Hspec

main :: IO ()
main = hspec spec

val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True)]

val' :: Value
val' = object [
  "boolean" .= True,
  "numbers" .= ([1,2,3] :: [Int]) ]

spec :: Spec
spec =
  describe "Working Directly with JSON" $ do
    it "can encode the JSON object" $ do
      let result = TL.decodeUtf8 . encode $ val
      result `shouldBe` "{\"boolean\":true,\"numbers\":[1,2,3]}"
    it "can use object to build up JSON" $ do
      let result = TL.decodeUtf8 . encode $ val'
      result `shouldBe` "{\"boolean\":true,\"numbers\":[1,2,3]}"
