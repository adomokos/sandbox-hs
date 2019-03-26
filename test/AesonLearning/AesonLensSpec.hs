module AesonLearning.AesonLensSpec where

import Test.Hspec

import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = hspec spec

jsonFile :: FilePath
jsonFile = "test/assets/employee.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

spec :: Spec
spec = do
  describe "Aeson Lens" $ do
    json <- runIO $ getJSON
    it "can read fields" $ do
      let firstDepartment =
            json ^? key "data" . key "person" . key "departments" . nth 0 .
            key "name"
      firstDepartment `shouldBe` Just "Human Resources"
      let age = json ^? key "data" . key "person" . key "age"
      age `shouldBe` Just (A.Number 28)
