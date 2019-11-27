module Conduit.SmartDataSpec where

{-
    Examples from here: https://mmhaskell.com/blog/2017/5/29/smart-data-with-conduits
-}

import Test.Hspec

import Conduit (conduitVector)
import Control.Monad.Identity (Identity)
import Data.Conduit (ConduitT, await, runConduit, runConduitPure, yield, (.|))
import Data.Conduit.List (sourceList)
import Data.Vector hiding (sum)
import qualified Data.Vector as Vec


main :: IO ()
main = hspec spec

myIntegerSourceBad :: [Integer]
myIntegerSourceBad = [1..1000]

myIntegerSource :: (Monad m, Num a, Enum a) => ConduitT i a m ()
myIntegerSource = sourceList [1..1000]

myIntegerSourceIO :: ConduitT i Integer IO ()
myIntegerSourceIO = sourceList [1..1000]

sumFromSource :: [Integer] -> Integer
sumFromSource lst = sum lst

myIntegerSink :: (Monad m, Num t) => t -> ConduitT t o m t
myIntegerSink accumV = do
  maybeFirstVal <- await
  case maybeFirstVal of
    Nothing -> pure accumV
    Just val -> do
      let newSum = val + accumV
      myIntegerSink newSum

myDoublingConduit :: ConduitT Integer Integer Identity ()
myDoublingConduit = do
  maybeVal <- await
  case maybeVal of
    Nothing -> pure ()
    Just val -> do
      let newVal = if val `mod` 2 == 0
            then val * 2
            else val
      yield newVal
      myDoublingConduit

myIntegerVectorSink :: Integer -> ConduitT (Vector Integer) o IO Integer
myIntegerVectorSink accumV = do
  maybeFirstVal <- await
  case maybeFirstVal of
    Nothing -> pure accumV
    Just vals -> do
      let newSum = Vec.sum vals + accumV
      -- lift $ print newSum
      myIntegerVectorSink newSum

fullConduit :: Integer
fullConduit = runConduitPure $
  myIntegerSource .| myIntegerSink 0

fullDoublingConduit :: Integer
fullDoublingConduit = runConduitPure $
  myIntegerSource .| myDoublingConduit .| myIntegerSink 0

fullConduitIO :: IO Integer
fullConduitIO = runConduit $
  myIntegerSourceIO .| conduitVector 100 .|
    myIntegerVectorSink 0

spec :: Spec
spec = do
  describe "Conduit Intro" $ do
    it "can consume and sum up large lists" $ do
      sumFromSource myIntegerSourceBad `shouldBe` 500500
    it "can consume and sum up large lists with Conduit" $ do
      fullConduit `shouldBe` 500500
    it "can take another Conduit as a plugin" $ do
      fullDoublingConduit `shouldBe` 751000
    it "can batch numbers" $ do
      result <- fullConduitIO
      result `shouldBe` 500500
