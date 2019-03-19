{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Reflection.IntroSpec where

-- https://chrisdone.com/posts/data-typeable

import Test.Hspec
import Data.Typeable
import Data.Data

main :: IO ()
main = hspec spec

data X = X { foo :: Int
           , bar :: Char }
           deriving (Typeable, Data, Show, Eq)

spec :: Spec
spec = do
  describe "Typeable - metaprogramming" $ do
    it "can print a type of something" $ do
      let t = typeOf 'a'
      show t `shouldBe` "Char"
    it "can compare the types of two things" $
      typeOf 'a' == typeOf 'b' `shouldBe` True
    it "can reify from generic -> concrete" $ do
      let char x = case cast x of
                     Just (c :: Char) -> show c
                     Nothing -> "unknown"
      char 'a' `shouldBe` "'a'"
      char (5 :: Int) `shouldBe` "unknown"
      char () `shouldBe` "unknown"

  describe "Data.Data has more info" $ do
    it "1. can get the date type" $ do
      -- dataTypeOf :: Data a => a -> DataType
      let dt = dataTypeOf (Just 'a')
      dataTypeName dt `shouldBe` "Maybe"
      show dt `shouldBe`
        "DataType {tycon = \"Maybe\", datarep = AlgRep [Nothing,Just]}"
    it "2. can inspect a data type" $ do
      (dataTypeOf (Just 'a')) `shouldSatisfy` isAlgType
      (dataTypeOf 'a') `shouldNotSatisfy` isAlgType
    it "3. can get the constructor of a value" $ do
      -- You can't do much with the constructor as-is,
      -- but compare and print it
      let constr = toConstr (Just 'a')
      show constr `shouldBe` "Just"
      let nothingConstr = toConstr (Nothing :: Maybe Char)
      constr == nothingConstr `shouldBe` False
      let constructorType = constrType (toConstr (Just 'a'))
      dataTypeName constructorType `shouldBe` "Maybe"
    it "4. can get fields of a constructor" $ do
      let xConstr = toConstr (X 0 'a')
      constrFields xConstr `shouldBe` ["foo", "bar"]
