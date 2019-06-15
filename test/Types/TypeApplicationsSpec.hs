{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Types.TypeApplicationsSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

class Convertable a where
  canConvert :: a -> Bool

class Loadable a where
  load :: String -> a

data User = User
  { userFirstName :: String
  , userLastName :: String }

instance Convertable User where
  canConvert (User fn ln) =
    length fn > 5 && length ln > 5

instance Loadable User where
  load input = User input "Bettencourt"

newtype Department = Department
  { depName :: String }

instance Convertable Department where
  canConvert (Department name) =
    length name > 8

instance Loadable Department where
  load input = Department input

canBeConverted :: forall a . (Convertable a, Loadable a) => String -> IO Bool
canBeConverted input = do
  let entity = load @a input
  pure $ canConvert @a entity

-- Examples
idString :: String -> String
idString = id @String

incShow :: forall a . (Read a, Show a, Num a) => String -> String
incShow = show . (+ 1) . read @a

showF :: String -> String
showF s = show (read @Int s)

spec :: Spec
spec = do
  describe "Type Application" $ do
    it "works with Type arguments" $ do
      id @String "a" `shouldBe` "a"
      id @Int 3 `shouldBe` 3

      incShow @Int "3" `shouldBe` "4"
      incShow @Double "3.4" `shouldBe` "4.4"

    it "checks if entity can be converted" $ do
      user <- canBeConverted @User "Johnny"
      user `shouldBe` True
      user' <- canBeConverted @User "John"
      user' `shouldBe` False

      dep <- canBeConverted @Department "HR"
      dep `shouldBe` False
      dep' <- canBeConverted @Department "Human Resources"
      dep' `shouldBe` True
