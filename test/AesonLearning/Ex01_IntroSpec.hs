module AesonLearning.Ex01_IntroSpec where

import Data.Aeson (decode, eitherDecode, encode)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Very basic decoding and encoding" $ do
    it "encodes a list" $
      encode ([1,2,3] :: [Int]) `shouldBe` "[1,2,3]"
    it "decodes a list" $
      (decode "[1,2,3]" :: Maybe [Integer])
        `shouldBe` Just [1,2,3]
    it "gets Nothing with error" $
      (decode "foo" :: Maybe [Integer])
        `shouldBe` Nothing
    it "lists parse error with eitherDecode" $ do
      let (Left errorMsg) =
            eitherDecode "[]" :: Either String Integer
      errorMsg `shouldStartWith` "Error in $:"
    it "helps with specific errors" $ do
      let (Left errorMsg) =
            eitherDecode "[1,2,[3,4]]" :: Either String (Int, Int, (Int, Bool))
      errorMsg `shouldContain` "expected Bool, but encountered Number"
    it "parses properly with eitherDecode" $ do
      let (Right x) =
            eitherDecode "[1,2,[3,true]]" :: Either String (Int, Int, (Int, Bool))
      x `shouldBe` (1,2,(3,True))
