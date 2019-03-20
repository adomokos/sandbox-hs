{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Reflection.IntroSpec where

-- https://chrisdone.com/posts/data-typeable

import Test.Hspec
import Data.Typeable
import Data.Data
import Control.Monad.State (execState, modify, forM_, evalState, get)
import Data.Maybe (fromJust)
import Data.Char (isUpper)

main :: IO ()
main = hspec spec

data X = X { foo :: Int
           , bar :: Char }
           deriving (Typeable, Data, Show, Eq)

constructXWithState :: X
constructXWithState =
  evalState
    (fromConstrM
      (do i <- get
          modify (+1)
          return
            (case i of
               0 -> fromConstr (toConstr (5::Int))
               1 -> fromConstr (toConstr 'b')
               _ -> error $ "Unmatched i for " ++ show i))

      (toConstr (X 4 'a')))
    0 :: X

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
      constrFields xConstr `shouldMatchList` ["foo", "bar"]

    it "5. can make a real value from its constructor" $ do
      let result = fromConstr (toConstr (Nothing :: Maybe ())) :: Maybe ()
      result `shouldBe` Nothing
      let justResult = fromConstrB (fromConstr (toConstr (2 :: Int)))
                                   (toConstr (Just 2 :: Maybe Int)) :: Maybe Int
      justResult `shouldBe` Just 2

    it "can make a real value with more than one value in constructor" $ do
      -- with the State monad
      execState (modify (+1)) 2 `shouldBe` 3
      execState (forM_ [1..5] (const (modify (+1)))) 5
        `shouldBe` 10
      constructXWithState `shouldBe` X 5 'b'

    it "6. can map over data structures generically" $ do
      let mapFn d = case cast d of
                      Nothing -> d
                      Just x -> fromJust (cast (if isUpper x then '!' else x))
      let result = gmapT mapFn (X 4 'a')
          result' = gmapT mapFn (X 4 'A')
      result `shouldBe` X 4 'a'
      result' `shouldBe` X 4 '!'

    it "7. can generate values from data structures" $ do
      let result = gmapQ (\d -> toConstr d) (X 5 'a')
      -- result `shouldBe` [5,'a']
      showConstr (head result) `shouldBe` "5"
      showConstr (last result) `shouldBe` "'a'"
