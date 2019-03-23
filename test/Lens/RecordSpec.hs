{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lens.RecordSpec where

import Test.Hspec

import Control.Lens
import Prelude hiding (log)

data Env = Env {
    envLog :: !String
  , envBalance :: !Int
  } deriving (Show, Eq)

makeLensesWith camelCaseFields ''Env

main :: IO ()
main = hspec spec

spec :: SpecWith()
spec =
  describe "Lens for Records" $
    it "works with regular fields" $ do
      let env = Env "the log" 100
      env ^. log `shouldBe` "the log"
      env ^. balance `shouldBe` 100
      let res = (balance .~ 110) env
      res `shouldBe` Env "the log" 110
      let res' = set balance 111 env
      res' `shouldBe` Env "the log" 111
