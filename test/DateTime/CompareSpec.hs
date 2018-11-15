{-# LANGUAGE OverloadedStrings #-}
module DateTime.CompareSpec where

import           Test.Hspec
import           Data.Maybe (fromJust)
import Control.Exception (evaluate)
import qualified Data.Text      as T
import qualified Data.Hourglass as H
import qualified Data.Time      as DT

main :: IO ()
main = hspec spec

-- dateParse :: T.Text -> H.Date
-- dateParse candidate =
  -- case H.timeParse H.ISO8601_Date sDateTime of
      -- Just dt -> Just (H.dtDate dt)
      -- Nothing -> Nothing
  -- where sDateTime = T.unpack candidate

spec :: Spec
spec =
  describe "Comparing various date time parsing" $ do
    it "can parse incorrect date time with Hourglass" $ do
      let date =
            H.dtDate . fromJust $ H.timeParse H.ISO8601_Date "2018-02-31"

      -- oops
      date `shouldBe` H.Date 2018 H.February 31

    it "can properly parse string to incorrect DateTime" $ do
      let strDateTime = "2018-02-31"
          result = DT.parseTimeM True DT.defaultTimeLocale "%Y-%m-%d" strDateTime :: Either String DT.UTCTime

      evaluate(result) `shouldThrow` anyErrorCall
