{-# LANGUAGE OverloadedStrings #-}
module Strings.IntroSpec where

import Test.Hspec

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.String (IsString(..))

main :: IO ()
main = hspec spec

newtype MyType = MyType String deriving (Eq, Show)

instance Data.String.IsString MyType where
  fromString = MyType

spec :: Spec
spec = do
  describe "Regular string" $ do
    it "is inefficient" $ do
      let a = "Hello" :: String
      map C.toUpper a `shouldBe` "HELLO"
      'x' : a `shouldBe` "xHello"
      a ++ " Person!" `shouldBe` "Hello Person!"
      L.sort "dbca" `shouldBe` "abcd"
    it "takes 4 string allocations" $ do
      let myFirstString = "Hello" :: String
          modifiedString =
            map C.toLower (L.sort (myFirstString ++ " Person!"))
      modifiedString `shouldBe` " !hpeellnoors"

  describe "Text - there is Strict and Lazy" $
    it "will only allocate a single Text objects" $ do
      let optimizedTextVersion =
            T.cons 'c' (T.map C.toLower (T.append "Hello"
                                                  " Person!")) :: T.Text
      optimizedTextVersion `shouldBe` "chello person!"

  -- More common than Text values, they are the lowest level
  -- representation of a character.
  -- Most networking libraries will use bytestring as they make the most
  -- sense for serialization
  describe "ByteString - there is Strict and Lazy" $
    it "works the same way as Text" $ do
      let x = "Hello!" :: BS.ByteString
      x `shouldBe` C8.pack "Hello!"

  describe "Conversions:" $ do
    it "pack :: String -> Text" $ do
      let x = "Hello!" :: String
      T.pack x `shouldBe` "Hello!"
    it "unpack :: Text -> String" $ do
      let x = T.pack "Hello!"
      T.unpack x `shouldBe` "Hello!"
    it "pack :: String -> ByteString" $ do
      let x = C8.pack "Hello!"
      x `shouldBe` ("Hello!" :: BS.ByteString)
    it "unpack :: ByteString -> String" $ do
      let x = C8.pack "Hello!"
      BS.unpack x `shouldBe` [72,101,108,108,111,33]

  describe "Convert between Lazy and Strict" $ do
    it "toStrict :: Data.Text.Lazy.Text -> Data.Text.Text" $ do
      let x = LT.pack "Hello!"
      LT.toStrict x `shouldBe` ("Hello!" :: T.Text)
    it "fromStrict :: Data.Text.Text -> Data.Text.Lazy.Text" $ do
      let x = "Hello!" :: T.Text
      LT.fromStrict x `shouldBe` LT.pack "Hello!"

  -- Going from Text to ByteString is straightforward, assuming
  -- you know your data format
  describe "Encoding" $ do
    it "encodeUtf8 :: Text -> ByteString" $ do
      let x = TE.encodeUtf8 "Hello!"
      x `shouldBe` ("Hello!" :: BS.ByteString)
    it "decodeUtf8 :: ByteString -> Text" $ do
      let x = C8.pack "Hello!"
      TE.decodeUtf8 x `shouldBe` "Hello!"

  describe "Custom type" $
    it "can have an IsString implementation" $ do
      let myTypeAsString = "Hello" :: MyType
      myTypeAsString `shouldBe` Data.String.fromString "Hello"
