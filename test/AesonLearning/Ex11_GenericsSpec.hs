{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.Ex11_GenericsSpec where

import Test.Hspec

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic)

main :: IO ()
main = hspec spec

data Person = Person {
  name :: String,
  age :: Int }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

spec :: Spec
spec = do
  describe "Using Generics for JSON serialization" $ do
    let genericJson = "{\"age\":25,\"name\":\"John Smith\"}"

    it "can deserialize it" $ do
      let (Just person) = decode genericJson :: Maybe Person
      person `shouldBe` Person "John Smith" 25

    it "can serialize it" $ do
      let p = Person "John Smith" 25
      encode p `shouldBe` genericJson
