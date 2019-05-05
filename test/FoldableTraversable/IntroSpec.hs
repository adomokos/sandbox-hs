module FoldableTraversable.IntroSpec where

import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "NonEmpty" $ do
    it "is foldable" $ do
      sum (NE.fromList [1 .. 5]) `shouldBe` 15
      foldMap Sum (NE.fromList [1 .. 5]) `shouldBe` Sum 15

      let greaterThanTwo x = if x > 2 then [x] else []
      foldMap greaterThanTwo (NE.fromList [1 .. 5]) `shouldBe` [3, 4, 5]

    it "is traversable" $ do
      let greaterThanThree x = if x > 3 then Just x else Nothing

      traverse greaterThanThree (NE.fromList [1 .. 5]) `shouldBe` Nothing
      traverse greaterThanThree (NE.fromList [4 .. 7])
        `shouldBe` NE.nonEmpty [4 .. 7]
