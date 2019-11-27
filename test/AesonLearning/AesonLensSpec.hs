{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.AesonLensSpec where

import Test.Hspec

import Control.Lens ((^?))
import qualified Data.Aeson as A
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Lazy as BSL
import GHC.Exts (fromList)
import GHC.Generics (Generic)

main :: IO ()
main = hspec spec

jsonFile :: FilePath
jsonFile = "test/assets/employee.json"

getJSON :: IO BSL.ByteString
getJSON = BSL.readFile jsonFile

expectedFirstDepartment :: A.Value
expectedFirstDepartment =
  A.Object $ fromList [
  ("name", A.String "Help Desk"),
  ("active", A.Bool False),
  ("code", A.String "D38-214-89")]

data Department = Department {
    name :: !String
  , code :: !String
  , active :: !Bool
  } deriving (Show, Eq, Generic)

instance A.FromJSON Department

spec :: Spec
spec =
  describe "Aeson Lens" $ do
    json <- runIO getJSON
    it "can read fields" $ do
      let age = json ^? key "data" . key "person" . key "age"
      age `shouldBe` Just (A.Number 28)
      let firstDepartmentName =
            json ^? key "data" . key "person" . key "departments" . nth 0 .
            key "name"
      firstDepartmentName `shouldBe` Just "Human Resources"

    it "can return entire JSON Segments" $ do
      let firstDepartment =
            json ^? key "data" . key "person" . key "departments" . nth 1
          expectedDept = A.fromJSON expectedFirstDepartment :: A.Result Department
          dept = A.fromJSON <$> firstDepartment :: Maybe (A.Result Department)
      dept `shouldBe` Just expectedDept
