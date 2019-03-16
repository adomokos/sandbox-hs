{-# LANGUAGE DeriveDataTypeable #-}
module Records.MetaprogrammingSpec where

import Test.Hspec
import Data.Data

data Red = Rec {
  alpha :: Int,
  beta :: Double,
  phi :: Float
  } deriving (Data, Typeable)

sample :: Red
sample = Rec 1 2.3 4.5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Metaprogramming on records" $ do
    it "works like this:" $ do
      let fields = constrFields . toConstr $ sample
      fields `shouldBe` ["alpha","beta","phi"]
