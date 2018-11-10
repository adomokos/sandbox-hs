{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AesonLearning.Ex04_RecordsSpec where

import Test.Hspec
import Data.Aeson

main :: IO ()
main = hspec spec

-- This is in the example documentation
data Person =
  Person {name :: String, age :: Int} deriving (Show, Eq)

instance FromJSON Person where
  parseJSON = withObject "person" $ \o ->
    Person <$> o .: "name" <*> o .: "age"

instance ToJSON Person where
  toJSON p = object [
    "name" .= name p,
    "age"  .= age p ]

-- Using RecordWildCards
data PersonRW =
  PersonRW { nameRW :: String, ageRW :: Int } deriving (Show, Eq)

instance FromJSON PersonRW where
  parseJSON = withObject "person" $ \o -> do
    nameRW <- o .: "name"
    ageRW  <- o .: "age"
    return PersonRW{..}

instance ToJSON PersonRW where
  toJSON PersonRW{..} = object [
    "name" .= nameRW,
    "age"  .= ageRW]

-- Using PostProcessing
data PersonPP = PersonPP { namePP :: String, agePP :: Int } deriving (Show, Eq)

instance FromJSON PersonPP where
  parseJSON = withObject "person" $ \o -> do
    firstName <- o .: "name"
    lastName  <- o .: "surname"
    let namePP = firstName ++ " " ++ lastName
    agePP     <- o .: "age"
    return PersonPP {..}

-- Optional fields
data PersonOF =
  PersonOF { nameOF :: String, ageOF :: Int } deriving (Show, Eq)

instance FromJSON PersonOF where
  parseJSON = withObject "person" $ \o -> do
    nameOF <- o .: "name"
    ageOF  <- o .:? "age" .!= 18
    return PersonOF {..}

spec :: Spec
spec = do
  describe "Serialize Record to/from JSON" $ do
    let p = Person "John" 23

    it "can encode the JSON object" $ do
      let sjson = encode p
      sjson `shouldBe` "{\"age\":23,\"name\":\"John\"}"

    it "can decode into a record" $ do
      let sjson = encode p
      (decode sjson :: Maybe Person)
        `shouldBe` Just p

  describe "Serialize Record to/from JSON with Record Wildcards" $ do
    let p = PersonRW "John" 23

    it "can encode the JSON object" $ do
      let sjson = encode p
      sjson `shouldBe` "{\"age\":23,\"name\":\"John\"}"

    it "can decode into a record" $ do
      let sjson = encode p
      (decode sjson :: Maybe PersonRW)
        `shouldBe` Just p

  describe "Serialize Record to/from JSON with post processing" $ do
    let p = PersonPP "John Smith" 23

    it "can decode into a record" $ do
      let sjson = "{\"age\":23,\"name\":\"John\",\"surname\":\"Smith\"}"

      (decode sjson :: Maybe PersonPP)
        `shouldBe` Just p

  describe "Can use optional values in parsing" $ do
    let p = PersonOF "John" 18

    it "can decode into a record" $ do
      let sjson = "{\"age\":null,\"name\":\"John\"}"

      (decode sjson :: Maybe PersonOF)
        `shouldBe` Just p

      let sjson' = "{\"name\":\"John\"}"
      (decode sjson' :: Maybe PersonOF)
        `shouldBe` Just p
