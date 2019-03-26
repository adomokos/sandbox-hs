{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module AesonLearning.AesonLensSpec where

import Test.Hspec

import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import Data.Maybe (fromJust)

main :: IO ()
main = hspec spec

jsonFile :: FilePath
jsonFile = "test/assets/employee.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

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
spec = do
  describe "Aeson Lens" $ do
    json <- runIO $ getJSON
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
          expectedDept = (A.fromJSON expectedFirstDepartment :: A.Result Department)
          dept = (A.fromJSON (fromJust firstDepartment) :: A.Result Department)
      expectedDept `shouldBe` dept
