{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module AesonLearning.Ex13_TemplateHaskellSpec where

import HereDoc
import Test.Hspec
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L8

data Person = Person {
  name :: String,
  age  :: Int }
  deriving (Show, Eq)

data Book = Book {
  title  :: String,
  author :: String }
  deriving (Show, Eq)

concat <$> mapM (deriveJSON defaultOptions) [''Person, ''Book]

trJson :: L8.ByteString
trJson = [heredoc|
{
  "data":[
    {
      "name": "Harold",
      "age": 20
    },
    {
      "name": "Maude",
      "age": 80
    }
  ]
}
|]

people :: Value -> Parser [Person]
people = withObject "people" $ \o -> o .: "data"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Using TemplateHaskell" $ do
    it "generates To/From parsers" $ do
      let sJson = "{\"name\":\"John Smith\",\"age\":25}"
          (Just person) = decode sJson :: Maybe Person
          p = Person "John Smith" 25

      person `shouldBe` p

      encode p `shouldBe` sJson

    it "can parse multiple fields at once" $ do
      let (Just [ppl1, ppl2]) =
            parseMaybe people =<< decode trJson :: Maybe [Person]

      ppl1 `shouldBe` Person "Harold" 20
      ppl2 `shouldBe` Person "Maude" 80
