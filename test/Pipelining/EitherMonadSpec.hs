{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Pipelining.EitherMonadSpec where

import           Test.Hspec
import qualified Data.Text as T
import           Debug.Trace (traceShow)

main :: IO ()
main = hspec spec

traceShow' :: Show b => b -> b
traceShow' arg = traceShow ("\n\n:: tracing :: - " <> (show arg) <> "\n\n") arg

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int } deriving (Show, Eq)

validateFirstName :: Person -> Either T.Text Person
validateFirstName p@Person {..}
  | firstName == "" = Left "No firstname"
  | otherwise = Right $ p -- to print it out use `traceShow' p`

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
