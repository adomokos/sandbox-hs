{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AesonLearning.Ex08_ExtendedRecordsSpec where

import Data.Aeson
import GHC.Exts (fromList)
import Test.Hspec

main :: IO ()
main = hspec spec

data Name = Name {
  firstname :: String,
  lastname  :: String
} deriving (Show, Eq)

data Person = Person {
  name :: Name,
  age :: Int
} deriving (Show, Eq)

instance FromJSON Name where
  parseJSON = withObject "name" $ \o -> do
    firstname <- o .: "firstname"
    lastname  <- o .: "surname"
    pure Name {..}

instance ToJSON Name where
  toJSON Name{..} = object [
    "firstname" .= firstname,
    "surname"   .= lastname ]

instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- parseJSON (Object o)
    age  <- o .: "age"
    pure Person {..}

instance ToJSON Person where
  toJSON Person{..} = Object $
    toObject name <>
    fromList ["age" .= age]

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

spec :: Spec
spec =
  describe "Parsing Extended Records" $ do
    let sjson = "{\"age\":38,\"firstname\":\"John\",\"surname\":\"Smith\"}"
        name = Name "John" "Smith"
        p = Person name 38

    it "combines parsers for deserialization" $ do
      let (Just person) = decode sjson :: Maybe Person
      person `shouldBe` p

    it "combines parsers for serialization" $ do
      let gjson = encode p
      gjson `shouldBe` sjson
