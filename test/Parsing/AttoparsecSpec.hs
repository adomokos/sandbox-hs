{-# LANGUAGE OverloadedStrings #-}
module Parsing.AttoparsecSpec where

-- Example from here: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.Time as DT
import Data.Word
import Test.Hspec

-- | Types for the log file content
data Product = Mouse | Keyboard | Monitor | Speakers
  deriving (Eq, Show, Enum)

data LogEntry =
  LogEntry { entryTime :: DT.LocalTime
           , entryIP :: IP
           , entryProduct :: Product
           , entrySource :: Source
           } deriving (Eq, Show)

type Log = [LogEntry]

-- | Type for IPs.
data IP = IP Word8 Word8 Word8 Word8 deriving (Eq, Show)

data Source = Internet | Friend | NoAnswer deriving (Eq, Show)

{- HLINT ignore ipParser -}
ipParser :: Parser IP
ipParser = do
  d1 <- decimal
  _ <- char '.'
  d2 <- decimal
  _ <- char '.'
  d3 <- decimal
  _ <- char '.'
  d4 <- decimal
  pure $ IP d1 d2 d3 d4

timeParser :: Parser DT.LocalTime
timeParser = do
  y <- count 4 digit
  _ <- char '-'
  mm <- count 2 digit
  _ <- char '-'
  d <- count 2 digit
  _ <- char ' '
  h <- count 2 digit
  _ <- char ':'
  m <- count 2 digit
  _ <- char ':'
  s <- count 2 digit
  pure $
    DT.LocalTime { DT.localDay = DT.fromGregorian (read y) (read mm) (read d)
                 , DT.localTimeOfDay = DT.TimeOfDay (read h) (read m) (read s) }

productParser :: Parser Product
productParser =
      (string "mouse" >> pure Mouse)
  <|> (string "keyboard" >> pure Keyboard)
  <|> (string "monitor" >> pure Monitor)
  <|> (string "speakers" >> pure Speakers)

sourceParser :: Parser Source
sourceParser =
      (string "internet" >> pure Internet)
  <|> (string "friend" >> pure Friend)
  <|> (string "noanswer" >> pure NoAnswer)

-- | Combining small parsers into one bigger one
logEntryParser :: Parser LogEntry
logEntryParser = do
  -- First, read in time
  t <- timeParser
  -- Followed by a space
  _ <- char ' '
  -- And then the IP of the client
  ip <- ipParser
  -- Followed by another space
  _ <- char ' '
  -- Finally, read in the type of product
  p <- productParser
  -- Look for the field 'Source' and return
  -- a default value ('NoAnswer') when missing.
  -- The arguments of 'option' are default
  -- value followed by the parser to try.
  s <- option NoAnswer $ char ' ' >> sourceParser
  -- And return the result as a value of type 'LogEntry'
  pure $ LogEntry t ip p s

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

-- Merging data from different logs
-- This is the European log format:
-- 154.41.32.99 29/06/2013 15:32:23 4 internet

-- Different kind of products are numbers from 1-4
-- in the given order
productFromID :: Int -> Product
productFromID n = toEnum (n-1)

productToID :: Product -> Int
productToID p = fromEnum p + 1

productParser2 :: Parser Product
productParser2 = productFromID . read . (:[]) <$> digit

timeParser2 :: Parser DT.LocalTime
timeParser2 = do
  d <- count 2 digit
  _ <- char '/'
  mm <- count 2 digit
  _ <- char '/'
  y <- count 4 digit
  _ <- char ' '
  h <- count 2 digit
  _ <- char ':'
  m <- count 2 digit
  _ <- char ':'
  s <- count 2 digit
  pure $
    DT.LocalTime { DT.localDay = DT.fromGregorian (read y) (read mm) (read d)
                 , DT.localTimeOfDay = DT.TimeOfDay (read h) (read m) (read s) }

{- HLINT ignore euParser -}
euParser :: Parser LogEntry
euParser = do
  ip <- ipParser
  _ <- char ' '
  t <- timeParser2
  _ <- char ' '
  p <- productParser2
  _ <- char ' '
  s <- sourceParser
  pure $ LogEntry t ip p s

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Attoparsec" $ do
    it "parses IP address strings" $ do
      let result = parseOnly ipParser "131.45.68.128"
      case result of
        Left err -> fail err
        Right s -> s `shouldBe` IP 131 45 68 128
    it "parses product info" $ do
      let (Right kb) = parseOnly productParser "keyboard"
      kb `shouldBe` Keyboard
      let (Right sp) = parseOnly productParser "speakers"
      sp `shouldBe` Speakers
    it "parses a log entry" $ do
      let logEntry = "2013-06-29 11:16:23 124.67.34.60 keyboard"
          (Right le) = parseOnly logEntryParser logEntry
      entryIP le `shouldBe` IP 124 67 34 60
      entryProduct le `shouldBe` Keyboard
    it "parses multiple log entries" $ do
      logEntries <- parseOnly logParser <$> BS.readFile "./resources/sellings.log"
      case logEntries of
        Left err -> fail err
        Right les -> length les `shouldBe` 12

  describe "Changes to the original parser" $
    it "parses out source" $ do
      let parsedData = parseOnly sourceParser "internet"
      parsedData `shouldBe` Right Internet
      let parsedData' = parseOnly sourceParser "noanswer"
      parsedData' `shouldBe` Right NoAnswer

  describe "Parsing European data formats" $ do
    it "leverages enums for Products" $ do
      productFromID 1 `shouldBe` Mouse
      productToID Keyboard `shouldBe` 2
    it "can parse number strings into Product" $ do
      let result = parseOnly productParser2 "4"
      result `shouldBe` Right Speakers
    it "can parse European date/time format" $ do
      let sampleDateTime = "29/06/2013 15:32:23"
          (Right result) = parseOnly timeParser2 sampleDateTime
      DT.localDay result `shouldBe` DT.fromGregorian 2013 6 29
    it "can parse a European logentry" $ do
      let sampleLog = "154.41.32.99 29/06/2013 15:32:23 4 internet"
          (Right result) = parseOnly euParser sampleLog
      entryProduct result `shouldBe` Speakers

