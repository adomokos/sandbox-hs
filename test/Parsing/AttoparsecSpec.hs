{-# LANGUAGE OverloadedStrings #-}
module Parsing.AttoparsecSpec where

-- Example from here: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

import Test.Hspec
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Internal as BI
import Data.Word
import qualified Data.Time as DT

logSampleContent :: BI.ByteString
logSampleContent =
  "2013-06-29 11:16:23 124.67.34.60 keyboard\n\
  \2013-06-29 11:32:12 212.141.23.67 mouse\n\
  \2013-06-29 11:33:08 212.141.23.67 monitor\n\
  \2013-06-29 12:12:34 125.80.32.31 speakers\n\
  \2013-06-29 12:51:50 101.40.50.62 keyboard\n\
  \2013-06-29 13:10:45 103.29.60.13 mouse"

-- | Types for the log file content
data Product = Mouse | Keyboard | Monitor | Speakers
  deriving (Eq, Show)

data LogEntry =
  LogEntry { entryTime :: DT.LocalTime
           , entryIP :: IP
           , entryProduct :: Product
           }
           deriving (Eq, Show)

type Log = [LogEntry]

-- | Type for IPs.
data IP = IP Word8 Word8 Word8 Word8 deriving (Eq, Show)

ipParser :: Parser IP
ipParser = do
  d1 <- decimal
  _ <- char '.'
  d2 <- decimal
  _ <- char '.'
  d3 <- decimal
  _ <- char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

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
      (string "mouse" >> return Mouse)
  <|> (string "keyboard" >> return Keyboard)
  <|> (string "monitor" >> return Monitor)
  <|> (string "speakers" >> return Speakers)

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
  -- And return the result as a value of type 'LogEntry'
  return $ LogEntry t ip p

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

main :: IO ()
main = hspec spec

spec :: Spec
spec =
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
      let logEntries = parseOnly logParser logSampleContent
      length logEntries `shouldBe` 1
