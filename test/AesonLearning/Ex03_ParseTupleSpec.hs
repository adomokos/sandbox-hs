{-# LANGUAGE OverloadedStrings #-}
module AesonLearning.Ex03_ParseTupleSpec where

import Data.Aeson (decode)
import Data.Aeson.Types
  (Parser, Value(..), parseJSON, parseMaybe, withArray, withObject, (.:))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

main :: IO ()
main = hspec spec

parseTuple :: Value -> Parser (String, Bool)
parseTuple (Object obj) = do
  -- look up the "a" field
  let mbFieldA = HashMap.lookup "a" obj

  -- fail if it wasn't found
  fieldA <- case mbFieldA of
    Just x -> pure x
    Nothing -> fail "no field 'a'"

  -- Extract the value from it, or fail if it's of the wrong type
  a <- case fieldA of
    String x -> pure (T.unpack x)
    _        -> fail "expected a string"

  -- Do all the same for "b" (in a slightly terser way, to save space):
  b <- case HashMap.lookup "b" obj of
    Just (Bool x) -> pure x
    Just _        -> fail "expected a boolean"
    Nothing       -> fail "no field 'b'"

  -- That's all!
  pure (a, b)
parseTuple _other = error "Does not matter for now"

parseTuple' :: Value -> Parser (String, Bool)
parseTuple' = withObject "tuple" $ \obj -> do
  -- Parse "a"
  a <- case HashMap.lookup "a" obj of
    Just x -> parseJSON x
    Nothing -> fail "no field 'a'"

  -- Parse "b"
  b <- case HashMap.lookup "b" obj of
    Just x -> parseJSON x
    Nothing -> fail "no field 'b'"

  -- That's all!
  pure (a, b)

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

    it "works with the most verbose solution" $ do
      let y = parseMaybe (parseArray parseTuple) =<< decode s
      y `shouldBe` Just [("hello",True),("word",False)]

    it "works with HashMap accessors" $ do
      let y = parseMaybe (parseArray parseTuple') =<< decode s
      y `shouldBe` Just [("hello",True),("word",False)]

    it "works with .: operator" $ do
      let y = parseMaybe (parseArray parseTuple'') =<< decode s
      y `shouldBe` Just [("hello",True),("word",False)]

    it "works with applicative style" $ do
      let y = parseMaybe (parseArray parseTuple''') =<< decode s
      y `shouldBe` Just [("hello",True),("word",False)]
