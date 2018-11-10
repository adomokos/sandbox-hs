{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AesonLearning.Ex07_NestedRecordsSpec where

import Test.Hspec
import Data.Aeson
import Data.ByteString.Lazy.Internal

main :: IO ()
main = hspec spec

jsonSample :: ByteString
jsonSample = "\
    \ { \
    \  \"name\":\"Nightfall\", \
    \  \"author\":{ \
    \      \"name\":\"Isaac Asimov\", \
    \      \"born\":1920 \
    \  } \
    \ }"

data Story = Story {
  name       :: String,
  author     :: String,
  authorBorn :: Int }
  deriving (Show, Eq)

instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> do
    name <- o .: "name"
    -- authorO :: Object
    authorO <- o .: "author"
    -- Now we can deconstruct authorO.
    author     <- authorO .: "name"
    authorBorn <- authorO .: "born"
    -- And finally return the value.
    pure Story { .. }

spec :: Spec
spec = do
  describe "Parsing with several record types" $ do
    it "picks the parser" $ do
      let (Just p) = (decode jsonSample :: Maybe Story)

      name p `shouldBe` "Nightfall"
      author p `shouldBe` "Isaac Asimov"
      authorBorn p `shouldBe` 1920
