{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module AesonLearning.Ex10_VerSpec where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , camelTo2
  , eitherDecode
  , object
  , parseJSON
  , toJSON
  , withObject
  , (.:)
  , (.=)
  )
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import GHC.Generics (Generic)
import HereDoc
import Safe (readMay)
import Test.Hspec

(?) :: Bool -> a -> a -> a
(?) True  t _ = t
(?) False _ f = f

infixl 1 ?

rjson :: BSL8.ByteString
rjson = [heredoc|
  [
      { "version":"3.0",
        "type":"pen"
      },
      {
        "version":"1.2",
        "type":"pencil"
      },
      {
        "version":"1.0",
        "occupation":"full_time"
      }
  ]
|]

data Typ = Pen
         | Pencil
         deriving (Generic, Show, Eq, Ord)

data Ver = V1_0
         | V1_1
         | V1_2
         | V3_0
           deriving (Generic, Show, Eq, Ord, Read)

data Vers = TypVer Typ Ver
          | OccVer Occupation Ver
            deriving (Generic, Show, Eq, Ord)

data Occupation = FullTime
                | PartTime
                deriving (Generic, Show, Eq, Ord)

instance ToJSON Occupation where
  toJSON p = String (T.pack $ camelTo2 '_' $ show p)

instance FromJSON Occupation where
  parseJSON x
    | x == "full_time" = pure FullTime
    | x == "part_time" = pure PartTime
    | otherwise        = fail $ "unrecognized value: " ++ show x

instance ToJSON Typ where
  toJSON typ = String (T.pack $ camelTo2 '_' $ show typ)

instance FromJSON Typ where
  parseJSON x
    | x == "pen"    = pure Pen
    | x == "pencil" = pure Pencil
    | otherwise     = fail $ "unrecognized value: " ++ show x

instance ToJSON Vers where
  toJSON (TypVer t v) =
    object [ "type" .= t
           , "version" .= v ]
  toJSON (OccVer p v) =
    object [ "occupation" .= p
             , "version" .= v]

instance FromJSON Vers where
  parseJSON = withObject "Vers" $ \o ->
    TypVer <$> o .: "type" <*> o .: "version" <|>
    OccVer <$> o .: "occupation" <*> o .: "version"

instance ToJSON Ver where toJSON v = String $ verVersion v
instance FromJSON Ver where
  parseJSON (String t) = let mv = mVersionToVer t
                           in case mv of
                                Nothing -> fail "Version does not match known values."
                                Just v -> pure v
  parseJSON _ = fail "Expected Version String"


verVersion :: Ver -> T.Text
verVersion v = T.pack . map (\c -> '_' == c ? '.' $ c) . filter ('V' /=) $ show v

mVersionToVer :: T.Text -> Maybe Ver
mVersionToVer t | T.length t > 100 = Nothing
                | otherwise = readMay str
  where str = T.unpack . T.map (\c -> '.' == c ? '_' $ c) $ T.cons 'V' t

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "can format to JSON" $ do
      let pjson = "{ \"version\":\"3.0\", \"type\":\"pen\"}"
          (Right r) = eitherDecode pjson :: Either String Vers
      r `shouldBe` TypVer Pen V3_0

      let cjson = "{ \"version\":\"3.0\", \"type\":\"pencil\"}"
          (Right r') = eitherDecode cjson :: Either String Vers

      r' `shouldBe` TypVer Pencil V3_0

      let prjson = "{ \"version\":\"1.2\", \"occupation\":\"part_time\"}"
          (Right r'') = eitherDecode prjson :: Either String Vers

      r'' `shouldBe` OccVer PartTime V1_2

    it "can parse a list of Vers" $ do
      let (Right r) = eitherDecode rjson :: Either String [Vers]
          expectedVers = [ TypVer Pen V3_0
                         , TypVer Pencil V1_2
                         , OccVer FullTime V1_0 ]
      r `shouldBe` expectedVers
