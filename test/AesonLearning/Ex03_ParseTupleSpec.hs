{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.Ex03_ParseTupleSpec where

import Data.Aeson (decode)
import Data.Aeson.Types
  (Parser, Value(..), parseMaybe, withArray, withObject, (.:))
import qualified Data.Vector as V
import Test.Hspec

main :: IO ()
main = hspec spec

parseTuple'' :: Value -> Parser (String, Bool)
parseTuple'' = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  pure (a, b)

parseTuple''' :: Value -> Parser (String, Bool)
parseTuple''' = withObject "tuple" $ \o ->
  (,) <$> o .: "a"
      <*> o .: "b"

parseArray :: (Value -> Parser b) -> Value -> Parser [b]
parseArray f = withArray "array of tuples" $ \arr ->
               traverse f (V.toList arr)

spec :: Spec
spec =
  describe "Parses a tuple with different formats" $ do
    let s = "[{\"a\": \"hello\", \"b\":true}, {\"a\":\"word\",\"b\":false}]"

    it "works with .: operator" $ do
      let y = parseMaybe (parseArray parseTuple'') =<< decode s
      y `shouldBe` Just [("hello",True),("word",False)]

    it "works with applicative style" $ do
      let y = parseMaybe (parseArray parseTuple''') =<< decode s
      y `shouldBe` Just [("hello",True),("word",False)]