module MonadTransformers.IntroSpec where

import Test.Hspec

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

run :: String -> IO String
run pwd = do
  password <- runMaybeT $ getPassword pwd
  case password of
    Just p -> pure $ "valid password: " <> p
    Nothing -> pure "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPwdLine :: String -> IO String
getPwdLine = pure

getPassword :: String -> MaybeT IO String
getPassword pwd = do
  password <- lift $ getPwdLine pwd
  guard (isValid password)
  pure pwd

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Monad Transformers" $ do
    it "can transform" $ do
      result <- run "some"
      result `shouldBe` "invalid password!"
      resultGood <- run "myLongPa$$word"
      resultGood `shouldBe` "valid password: myLongPa$$word"
