{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.Ex06_TypesWithManyConstructorSpec where

import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import Data.Foldable (asum)
import Test.Hspec

main :: IO ()
main = hspec spec

data Something
  = Person { name :: String, age :: Int }
  | Book { name :: String, author :: String }
  deriving (Show, Eq)

instance FromJSON Something where
  parseJSON = withObject "book or person" $ \o -> asum [
    Person <$> o .: "name" <*> o .: "age",
    Book <$> o .: "name" <*> o .: "author" ]

data Something'
  = Person' { name' :: String, age' :: Int }
  | Book' { name' :: String, author' :: String }
  deriving (Show, Eq)

instance FromJSON Something' where
  parseJSON = withObject "book or person" $ \o -> do
    kind <- o .: "kind"
    case kind of
      "person" -> Person' <$> o .: "name" <*> o .: "age"
      "book"   -> Book' <$> o .: "name" <*> o .: "author"
      _        -> fail ("unkown kind:" ++ kind)

spec :: Spec
spec = do
  describe "Parsing with several record types" $ do
    it "picks the parser" $ do
      let pjson = "{\"name\": \"John\",\"age\":25}"
          p = Person "John" 25

      (decode pjson :: Maybe Something)
        `shouldBe` Just p

      let bjson = "{\"name\": \"War and Peace\",\"author\":\"Lev T.\"}"
          b = Book "War and Peace" "Lev T."

      (decode bjson :: Maybe Something)
        `shouldBe` Just b

    it "can chose parser based on kind" $ do
      let pjson = "{\"kind\":\"person\",\"name\": \"John\",\"age\":25}"
          p = Person' "John" 25

      (decode pjson :: Maybe Something')
        `shouldBe` Just p

      let bjson = "{\"kind\":\"book\",\"name\": \"War and Peace\",\"author\":\"Lev T.\"}"
          b = Book' "War and Peace" "Lev T."

      (decode bjson :: Maybe Something')
        `shouldBe` Just b
