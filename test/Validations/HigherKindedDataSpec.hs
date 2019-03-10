{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
module Validations.HigherKindedDataSpec where

-- https://reasonablypolymorphic.com/blog/higher-kinded-data/

import Test.Hspec

import GHC.Generics (Generic)
import Control.Monad.Identity
import Data.Maybe (isNothing)

main :: IO ()
main = hspec spec

data Person = Person
  { pName :: String
  , pAge :: Int
  } deriving (Show, Eq, Generic)

-- One way to model a user form fill with a second datatype

data MaybePerson = MaybePerson
  { mpName :: Maybe String
  , mpAge :: Maybe Int
  } deriving (Show, Eq, Generic)

validate :: MaybePerson -> Maybe Person
validate (MaybePerson name age) =
  Person <$> name <*> age

-- This is tedious, we have to maintain 2 different types
-- Use higher kinded data (hkd) this way:

data Person' f = Person'
  { pName' :: f String
  , pAge' :: f Int
  } deriving (Generic)

type IdentityPerson' = Person' Identity -- deriving (Show, Eq)
type MaybePerson'    = Person' Maybe -- deriving (Show, Eq)

validate' :: MaybePerson' -> Maybe IdentityPerson'
validate' (Person' vpName' vpAge') =
  Person' <$> (Identity <$> vpName') <*> (Identity <$> vpAge')

-- Annoying, as all of our data is wrapped
-- up inside of an `Identity`
-- To get rid of Identity, we can use a type
-- family that erases them

-- "Higher-Kinded Data"
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data HkdPerson' f = HkdPerson
  { hkpName :: HKD f String
  , hkpAge  :: HKD f Int
  } deriving (Generic)

hkValidate :: HkdPerson' Maybe -> Maybe (HkdPerson' Identity)
hkValidate (HkdPerson name age) =
  HkdPerson <$> name <*> age

spec :: Spec
spec =
  describe "Validating Records" $ do
    it "works as Maybe data" $ do
      let mPerson = MaybePerson (Just "John") Nothing
      validate mPerson `shouldBe` Nothing
      let mPerson' = MaybePerson (Just "John") (Just 25)
      validate mPerson' `shouldBe` (Just $ Person "John" 25)

    it "works with sort-of higher-kinded Person" $ do
      let hkdPerson = Person' (Just "John") (Just 25)
          mPerson' = validate' hkdPerson
      pName' hkdPerson `shouldBe` Just "John"
      pName' <$> mPerson' `shouldBe` Just (Identity "John")
      pAge' <$> mPerson' `shouldBe` Just (Identity 25)

    it "works with higher-kinded type" $ do
      let hkdPerson = HkdPerson (Just "John") (Just 25) :: HkdPerson' Maybe
          (Just result) = hkValidate hkdPerson
      hkpName result `shouldBe` "John"
      hkpAge result `shouldBe` 25
      let hkdPersonWrong = HkdPerson (Just "John") Nothing :: HkdPerson' Maybe
          result' = hkValidate hkdPersonWrong
      isNothing result' `shouldBe` True
