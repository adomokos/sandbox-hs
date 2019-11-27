{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.Ex12_GenericsCustomizeSpec where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , camelTo2
  , decode
  , defaultOptions
  , encode
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , parseJSON
  , toJSON
  )
import Data.Char (toLower)
import GHC.Generics (Generic)
import Test.Hspec

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

data Person = Person {
  _name :: String,
  _age  :: Int
} deriving (Show, Eq, Generic)

instance ToJSON Person where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

-- Converts json keys from `legal_name` to `legalName`
data Employee = Employee {
  legalName :: String,
  minimumAge :: Int
} deriving (Show, Eq, Generic)

instance ToJSON Employee where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = camelTo2 '_' }

instance FromJSON Employee where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' }

-- Weird fields, can come in as random casing
data WFPerson = WFPerson {
  wfName :: String,
  wfAge  :: Int } deriving (Show, Eq, Generic)

instance FromJSON WFPerson where
  parseJSON = genericParseJSON opts . jsonLower
    where
      opts = defaultOptions { fieldLabelModifier = map toLower . drop 2 }

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HashMap.fromList . map lowerPair . HashMap.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

-- for more info, read about it
-- here: http://hackage.haskell.org/package/aeson-1.4.1.0/docs/Data-Aeson-Types.html#t:Options

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Using Generics with customization" $ do
    describe "- with leading underscore" $ do
      let cgJson = "{\"age\":25,\"name\":\"John Smith\"}"

      it "can deserialize it" $ do
        let (Just person) = decode cgJson :: Maybe Person
        person `shouldBe` Person "John Smith" 25

      it "can serialize it" $ do
        let p = Person "John Smith" 25
        encode p `shouldBe` cgJson

    describe "- with camel case transformer" $ do
      let cgJson = "{\"legal_name\":\"John Smith\",\"minimum_age\":25}"

      it "can deserialize it" $ do
        let (Just person) = decode cgJson :: Maybe Employee
        person `shouldBe` Employee "John Smith" 25

      it "can serialize it" $ do
        let p = Employee "John Smith" 25
        encode p `shouldBe` cgJson

    describe "- with weird casing in JSON keys" $ do
      let cgJson = "{\"aGE\":24,\"NaMe\":\"John Smith\"}"

      it "can deserialize it" $ do
        let (Just person) = decode cgJson :: Maybe WFPerson
        person `shouldBe` WFPerson "John Smith" 24
