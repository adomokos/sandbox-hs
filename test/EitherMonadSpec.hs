{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module EitherMonadSpec where

import           Test.Hspec
import qualified Data.Text as T

main :: IO ()
main = hspec spec

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int } deriving (Show, Eq)

validateFirstName :: Person -> Either T.Text Person
validateFirstName p@Person {..}
  | firstName == "" = Left "No firstname"
  | otherwise = Right p

validateLastName :: Person -> Either T.Text Person
validateLastName p@Person {..}
  | lastName == "" = Left "No lastname"
  | otherwise = Right p

spec :: Spec
spec =
  describe "Various ways to handle errors" $ do
    it "can pass a person through" $ do
      let p = Person "John" "Smith" 24
          (Right x) = pure p >>= validateFirstName >>= validateLastName

      x `shouldBe` p

    it "fails immediately" $ do
      let p = Person "" "Smith" 24
          (Left err) = pure p >>= validateFirstName >>= validateLastName

      err `shouldBe` "No firstname"

    it "fails for second check" $ do
      let p = Person "John" "" 24
          -- (Left err) = pure p >>= validateFirstName >>= validateLastName
          (Left err) = validateLastName =<< validateFirstName =<< pure p

      err `shouldBe` "No lastname"
