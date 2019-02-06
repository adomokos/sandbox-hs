{-# LANGUAGE OverloadedStrings #-}
module Parsing.AttoparsecSpec where

-- Example from here: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

import Test.Hspec
import Data.Attoparsec.ByteString.Char8
import Data.Word

-- | Type for IPs.
data IP = IP Word8 Word8 Word8 Word8 deriving (Eq, Show)

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  _ <- char '.'
  d2 <- decimal
  _ <- char '.'
  d3 <- decimal
  _ <- char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Attoparsec" $
    it "parses IP address strings" $ do
      let result = parseOnly parseIP "131.45.68.128"
      case result of
        Left err -> fail err
        Right s -> s `shouldBe` IP 131 45 68 128
