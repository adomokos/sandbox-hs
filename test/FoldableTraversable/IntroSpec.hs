module FoldableTraversable.IntroSpec where

import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Test.Hspec

main :: IO ()
main = hspec spec

newtype MySum a = MySum { getMySum :: a }
  deriving (Show, Eq)

instance Functor MySum where
  fmap f (MySum x) = MySum (f x)

instance (Num a) => Semigroup (MySum a) where
  (MySum x) <> (MySum y) = MySum (x + y)

instance (Num a) => Monoid (MySum a) where
  mempty = MySum 0

newtype FIdentity a = FIdentity a
  deriving (Show, Eq)

instance Foldable FIdentity where
  foldr f z (FIdentity x) = f x z
  foldl f z (FIdentity x) = f z x
  foldMap f (FIdentity x) = f x

greaterThanThree :: Int -> Maybe Int
greaterThanThree x | x > 3     = Just x
                   | otherwise = Nothing

spec :: Spec
spec = do
  describe "Operations with monoidal values" $ do
    it "can fold a list of Sum/Product values" $ do
      let sums          = map Sum [1 .. 5]
          sumResult     = fold sums
          products      = map Product [1 .. 5]
          productResult = fold products

      sumResult `shouldBe` Sum 15
      productResult `shouldBe` Product 120

    it "foldMap - maps each element to a Monoid" $ do
      foldMap Sum [1 .. 5] `shouldBe` Sum 15
      foldMap All [True, False, True] `shouldBe` All False
      foldMap Any [3 == 4, 9 > 5] `shouldBe` Any True

  describe "Monoidal instance of MySum" $ do
    it "works with MySum" $ do
      let result = foldMap MySum [1 .. 5]
      result `shouldBe` MySum 15

      foldMap MySum [] `shouldBe` MySum 0

  describe "Foldable of custom types" $ do
    it "works with FIdentity" $ do
      -- Why this one does not work?
      -- (foldMap (+ 2) (FIdentity 3) :: MySum Integer) `shouldBe` MySum 5
      (foldMap (+ 2) (FIdentity 3) :: Sum Integer) `shouldBe` Sum 5

  describe "Operations with NonEmpty" $ do
    it "is foldable" $ do
      sum (NE.fromList [1 .. 5]) `shouldBe` 15
      foldMap Sum (NE.fromList [1 .. 5]) `shouldBe` Sum 15

      let greaterThanTwo x = if x > 2 then [x] else []
      foldMap greaterThanTwo (NE.fromList [1 .. 5]) `shouldBe` [3, 4, 5]

    it "is traversable" $ do
      traverse greaterThanThree (NE.fromList [1 .. 5]) `shouldBe` Nothing
      traverse greaterThanThree (NE.fromList [4 .. 7])
        `shouldBe` NE.nonEmpty [4 .. 7]
