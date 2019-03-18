{-# LANGUAGE ScopedTypeVariables #-}
module Typeable.IntroSpec where

-- https://chrisdone.com/posts/data-typeable

import Test.Hspec
import Data.Typeable
import Data.Data

main :: IO ()
main = hspec spec

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
    it "can get the date type" $ do
      -- dataTypeOf :: Data a => a -> DataType
      let dt = dataTypeOf (Just 'a')
      dataTypeName dt `shouldBe` "Maybe"
      show dt `shouldBe`
        "DataType {tycon = \"Maybe\", datarep = AlgRep [Nothing,Just]}"
    it "can inspect a data type" $ do
      pending
