{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevelProgramming.IntroSpec where

-- https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html

import Test.Hspec

main :: IO ()
main = hspec spec

data TpMaybe a = TpJust a
               | TpNothing
               deriving (Show, Eq)

data TpHigherKinded f a
  = TpBare a
  | TpWrapped (f a)

data TpNat = TpZero
           | TpSucc TpNat
           deriving (Show, Eq)

data TpIntBool a where
  TpInt :: Int -> TpIntBool Int
  TpBool :: Bool -> TpIntBool Bool

instance (Eq a) => Eq (TpIntBool a) where
  (==) (TpInt  x) (TpInt  y) = x == y
  (==) (TpBool x) (TpBool y) = x == y

instance (Show a) => Show (TpIntBool a) where
  show (TpInt x) = "TpInt " <> show x
  show (TpBool x) = "TpBool " <> show x

data TpVector (n :: TpNat) (a :: *) where
  VNil :: TpVector 'TpZero a
  VCons :: a
        -> TpVector n a
        -> TpVector ('TpSucc n) a

instance Show a => Show (TpVector n a) where
  show VNil = "VNil"
  show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

instance Eq a => Eq (TpVector n a) where
  (==) VNil VNil = True
  (==) (VCons x _a) (VCons y _b) = x == y

add :: TpNat -> TpNat -> TpNat
add TpZero     n = n
add (TpSucc n) m = add n (TpSucc m)

type family Add n m where
  Add 'TpZero n = n
  Add ('TpSucc n) m = Add n ('TpSucc m)

spec :: Spec
spec = do
  describe "Type Level Programming" $ do
    it "works with kinds" $ do
      let nothingA    = TpNothing :: TpMaybe a
      let nothingInt  = TpNothing :: TpMaybe Int
      let nothingChar = TpNothing :: TpMaybe Char

      nothingA `shouldBe` nothingInt
      nothingA `shouldBe` nothingChar

    it "works with GADTs" $ do
      let x' = TpBool False
      let y' = TpBool True

      TpInt 3 == TpInt 4 `shouldBe` False
      TpInt 3 == TpInt 3 `shouldBe` True
      x' == y' `shouldBe` False
