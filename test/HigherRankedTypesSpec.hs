{-# LANGUAGE RankNTypes #-}
module HigherRankedTypesSpec where

-- https://8thlight.com/blog/mark-grant/2013/09/13/higher-ranked-types-part-1.html

import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.List (any)
import Data.Maybe (isJust)
import Test.Hspec

main :: IO ()
main = hspec spec

combine :: forall a b . (Show a, Show b) => a -> b -> String
combine x y = show x ++ show y

-- The forall introduces a new type variable scope within the function,
-- by creating the type variable c within that scope. The type checker no
-- longer attempts to unify it with any type variables that occur outside of
-- that scope.
-- We could have written the type signature this way:
-- :: (Show a, Show b) => (forall a . Show a => a -> String) -> a -> b -> String
-- Even though the type variable `a` is used twice, the type checker understands
-- the instance inside of the `forall` scope is different from the one
-- outside of it.
-- The only thing the compiler needs to verify about the input of `f` is that it
-- belongs to the `Show` typeclass.

customCombine
  :: (Show a, Show b)
  => (forall a' . Show a' => a' -> String)
  -> a
  -> b
  -> String
customCombine f x y = f x ++ f y

-- Kliesli Fish

longEnough :: String -> Maybe String
longEnough input | length input > 8 = Just input
                 | otherwise        = Nothing

hasNumber :: String -> Maybe String
hasNumber input | any isDigit input = Just input
                | otherwise         = Nothing

goodPassword :: String -> Bool
goodPassword pwd = isJust $ hasNumber =<< longEnough pwd

goodPasswordKliesli :: String -> Bool
goodPasswordKliesli = isJust . (hasNumber <=< longEnough)

data PasswordError
  = NotLongEnough
  | HasNoNumber
  deriving (Show, Eq)

longEnoughE :: String -> Either PasswordError String
longEnoughE input | length input > 8 = Right input
                  | otherwise        = Left NotLongEnough

hasNumberE :: String -> Either PasswordError String
hasNumberE input | any isDigit input = Right input
                 | otherwise         = Left HasNoNumber

goodPasswordKliesliE :: String -> Either PasswordError String
goodPasswordKliesliE = hasNumberE <=< longEnoughE

spec :: Spec
spec = do
  describe "Higher-Ranked types" $ do
    it "is implicitly inferred" $ do
      combine "Hello: " 14 `shouldBe` "\"Hello: \"14"
    it "creates a new scope for polymorphic functions" $ do
      let f x = show x ++ "!"
      customCombine f False 15 `shouldBe` "False!15!"

  describe "Kliesli Fish operator" $ do
    it "validates password with Maybe" $ do
      goodPassword "password" `shouldBe` False
      goodPassword "password1" `shouldBe` True
      goodPasswordKliesli "password" `shouldBe` False
      goodPasswordKliesli "password1" `shouldBe` True

    it "validates password with Either" $ do
      goodPasswordKliesliE "password" `shouldBe` Left NotLongEnough
      goodPasswordKliesliE "password1" `shouldBe` Right "password1"
