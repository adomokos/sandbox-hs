{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lens.RecordSpec where

import Test.Hspec

import Control.Lens
import Prelude hiding (log)

data LogVar = LogVar {
    logVarName :: !String
  , logVarPath :: !String
} deriving (Show, Eq)

makeLensesWith camelCaseFields ''LogVar

data Env = Env {
    envLog :: !LogVar
  , envBalance :: !Int
  } deriving (Show, Eq)

makeLensesWith camelCaseFields ''Env

main :: IO ()
main = hspec spec

logVar :: LogVar
logVar = LogVar "system" "/etc/log"

spec :: SpecWith()
spec =
  describe "Lens for Records" $
    it "works with regular fields" $ do
      let env = Env logVar 100
      env ^. log `shouldBe` logVar
      env ^. (log . name) `shouldBe` "system"
      env ^. balance `shouldBe` 100
      let res = (balance .~ 110) env
      res `shouldBe` Env logVar 110
      let res' = set balance 111 env
      res' `shouldBe` Env logVar 111
