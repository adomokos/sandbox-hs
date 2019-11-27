{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AesonLearning.Ex05_MoreInterestingChoicesSpec where

import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Aeson (FromJSON, decode, eitherDecode, parseJSON, withObject, (.:))
import Data.Foldable (asum)
import Test.Hspec
import Text.Read (readMaybe)

main :: IO ()
main = hspec spec

data Person =
  Person {name :: String, age :: Int} deriving (Show, Eq)

instance FromJSON Person where
  parseJSON = withObject "person" $ \o -> do
    name <- o .: "name"
    age <- o .: "age" <|> o .: "AGE"
    pure Person{..}

-- Choosing from a list of parsers
data PersonC =
  PersonC {nameC :: String, ageC :: Int} deriving (Show, Eq)

instance FromJSON PersonC where
  parseJSON = withObject "person" $ \o -> do
    nameC <- o .: "name"
    ageC <- asum [
      -- The simple number case
      o .: "age",
      -- The more complicated "string" case
      do s <- o .: "age"
         case readMaybe s of
           Nothing -> fail "not a number"
           Just x -> pure x,
      -- The "tuple" case.
      o .: "AGE", -- Couldn't get this to work
      -- The "John" case.
      do guard (nameC == "John")
         pure 24 ]
    pure PersonC{..}

data PersonG =
  PersonG {nameG :: String, ageG :: Int} deriving (Show, Eq)

instance FromJSON PersonG where
  parseJSON = withObject "person" $ \o -> do
    nameG <- o .: "name"
    when (nameG == "Ann") $
      fail "GO AWAY ANN"
    ageG <- o .: "age"
    pure PersonG{..}

spec :: Spec
spec = do
  describe "Looking at optional fields with Alternative" $ do
    it "uses age if found" $ do
      let sjson = "{\"age\":28,\"name\":\"John\"}"
          p = Person "John" 28

      (decode sjson :: Maybe Person)
        `shouldBe` Just p

    it "uses AGE if age not found" $ do
      let sjson = "{\"AGE\":32,\"name\":\"John\"}"
          p = Person "John" 32

      (decode sjson :: Maybe Person)
        `shouldBe` Just p

    it "uses no default - unsuccessful parsing" $ do
      let sjson = "{\"name\":\"John\"}"

      (decode sjson :: Maybe Person)
        `shouldBe` Nothing

  describe "More sophisticated parsing" $ do
    it "can parse out the happy path" $ do
      let sjson = "{\"age\":28,\"name\":\"John\"}"
          p = PersonC "John" 28

      (decode sjson :: Maybe PersonC)
        `shouldBe` Just p

    it "can parse out the age as string" $ do
      let sjson = "{\"age\":\"28\",\"name\":\"John\"}"
          p = PersonC "John" 28

      (decode sjson :: Maybe PersonC)
        `shouldBe` Just p

    it "can use AGE if age not found" $ do
      let sjson = "{\"AGE\":28,\"name\":\"Paul\"}"
          p = PersonC "Paul" 28

      (decode sjson :: Maybe PersonC)
        `shouldBe` Just p

    it "sets age to 24 for John" $ do
      let sjson = "{\"age\":null,\"name\":\"John\"}"
          p = PersonC "John" 24

      (decode sjson :: Maybe PersonC)
        `shouldBe` Just p

    it "returns GO AWAY ANN in the failure for name Ann" $ do
      let sjson = "{\"age\":24,\"name\":\"Ann\"}"

      let (Left x) = eitherDecode sjson :: Either String PersonG
      x `shouldBe` "Error in $: GO AWAY ANN"

    it "does not have error for non-Ann" $ do
      let sjson = "{\"age\":24,\"name\":\"Ann1\"}"
          p = PersonG "Ann1" 24

      (decode sjson :: Maybe PersonG)
        `shouldBe` Just p
