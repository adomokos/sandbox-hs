module TypeClasses.CustomTypesSpec where

import Test.Hspec
import Data.Monoid (Sum(..))

-- MyMaybe - like Maybe
data MyMaybe a = MyNothing
               | MyJust a
               deriving (Show, Eq)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
  pure x = MyJust x
  MyNothing <*> _ = MyNothing
  _ <*> MyNothing = MyNothing
  (MyJust f) <*> (MyJust x) = MyJust (f x)

instance Monad MyMaybe where
  return = pure
  MyNothing >>= _ = MyNothing
  MyJust x >>= f = f x

instance Foldable MyMaybe where
  foldr _ z MyNothing = z
  foldr f z (MyJust x) = f x z

  foldl _ z MyNothing = z
  foldl f z (MyJust x) = f z x

  foldMap _ MyNothing = mempty
  foldMap f (MyJust x) = f x

instance Traversable MyMaybe where
  traverse _ MyNothing = pure MyNothing
  traverse f (MyJust x) = MyJust <$> (f x)

-- MyEither - like Either
data MyEither a b = MyLeft a
                  | MyRight b
                  deriving (Show, Eq)

instance Functor (MyEither a) where
  fmap _ (MyLeft x) = MyLeft x
  fmap f (MyRight y) = MyRight (f y)

instance Applicative (MyEither a) where
  pure x = MyRight x
  (MyLeft x) <*> _ = MyLeft x
  _ <*> (MyLeft x) = MyLeft x
  (MyRight f) <*> (MyRight x) = MyRight (f x)

instance Monad (MyEither a) where
  return = pure
  MyLeft x >>= _ = MyLeft x
  MyRight x >>= f = f x

instance Foldable (MyEither a) where
  foldr _ z (MyLeft _) = z
  foldr f z (MyRight y) = f y z

  foldl _ z (MyLeft _) = z
  foldl f z (MyRight x) = f z x

  foldMap _ (MyLeft _) = mempty
  foldMap f (MyRight x) = f x

instance Traversable (MyEither a) where
  traverse _ (MyLeft x) = pure (MyLeft x)
  traverse f (MyRight y) = MyRight <$> f y

leftString :: MyEither String Int
leftString = MyLeft "Hello"

rightInt :: MyEither String Int
rightInt = MyRight 3

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TypeClass Custom Implementations" $ do
    context "Maybe-like MyMaybe" $ do
      it "works as Functor" $ do
        fmap (+2) MyNothing `shouldBe` (MyNothing :: MyMaybe Int)
        fmap (+2) (MyJust 3) `shouldBe` (MyJust 5 :: MyMaybe Int)
      it "works as Applicative" $ do
        (+) <$> (MyJust 2) <*> (MyJust 3 :: MyMaybe Int)
          `shouldBe` MyJust 5
        (+) <$> MyNothing <*> (MyJust 3 :: MyMaybe Int)
          `shouldBe` MyNothing
      it "works as Monad" $ do
        (pure 2 >>= (\x -> MyJust (x+3))) `shouldBe` (MyJust 5 :: MyMaybe Int)
        (MyNothing >>= (\x -> MyJust (x+3))) `shouldBe` (MyNothing :: MyMaybe Int)
        (pure (2 :: Int) >>= (\_ -> MyNothing)) `shouldBe` (MyNothing :: MyMaybe Int)
      it "works as Foldable" $ do
        let fm = foldMap (+1)
        (fm MyNothing :: Sum Integer)
          `shouldBe` Sum 0
        (fm (MyJust 3) :: Sum Integer)
          `shouldBe` Sum 4
      it "works as Traversable" $ do
        let tfn x = MyJust (x+1)
        traverse tfn (MyJust 3 :: MyMaybe Int)
          `shouldBe` MyJust (MyJust 4)
        traverse tfn (MyNothing :: MyMaybe Int)
          `shouldBe` MyJust (MyNothing)

    context "Either-like MyEither" $ do
      it "works as Functor" $ do
        fmap (+2) leftString `shouldBe` leftString
        fmap (+2) rightInt `shouldBe` MyRight 5
      it "works as Aplicative" $ do
        MyRight (+2) <*> rightInt
          `shouldBe` MyRight 5
        MyRight (+2) <*> leftString
          `shouldBe` leftString
        MyLeft "Hello" <*> rightInt
          `shouldBe` leftString
      it "works as Monad" $ do
        (leftString >>= (\x -> MyRight (x+3)))
          `shouldBe` MyLeft "Hello"
        ((pure 2 :: MyEither String Int) >>= (\x -> MyRight (x+3)))
          `shouldBe` (MyRight 5 :: MyEither String Int)
        ((pure 2 :: MyEither String Int) >>= (\_ -> leftString))
          `shouldBe` (MyLeft "Hello" :: MyEither String Int)
      it "works as Foldable" $ do
        let fm = foldMap (+1)
        (fm (MyLeft ("Hello" :: String)) :: Sum Integer)
          `shouldBe` Sum 0
        (fm (MyRight 4) :: Sum Integer)
          `shouldBe` Sum 5
      it "works as Traversable" $ do
        let tfn x = MyRight (x+1)
        traverse tfn (MyRight 3)
          `shouldBe` (MyRight (MyRight 4) :: MyEither String (MyEither String Int))

