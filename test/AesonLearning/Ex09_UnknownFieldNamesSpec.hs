{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module AesonLearning.Ex09_UnknownFieldNamesSpec where

import HereDoc
import Test.Hspec
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as L8

rjson :: L8.ByteString
rjson = [heredoc|
{
    "website1.com": {
        "/page1": 3,
        "/page2": 4
    },
    "website2.com": {
        "/page": 10
  }
}
|]

main :: IO ()
main = hspec spec

data Referer = Referer {
  domain :: String,
  pathAccesses :: [(String, Int)] }
  deriving (Show, Eq)

parseReferers :: Value -> Parser [Referer]
parseReferers p =
  -- Convert each "accesses" object to a list of pairs, and create a Referrer.
  map (\(domain, accesses) -> Referer domain (HM.toList accesses)) .
  -- Turn the HashMap into a list of (domain, accesses) pairs.
  -- Each "accesses" object looks like {"/page1": 3, ...}.
  HM.toList <$>
  -- Parse our data into a HashMap String (HashMap String Int).
  parseJSON p

spec :: Spec
spec = do
  describe "Parsing unknown field names" $ do
    it "accesses it " $ do
      let ojson = fromJust . decode $ rjson
          (Right ref) = (parseEither parseReferers ojson) :: Either String [Referer]
      length ref `shouldBe` 2
      let [ref1, ref2] = ref
      domain ref1 `shouldBe` "website1.com"
      domain ref2 `shouldBe` "website2.com"
