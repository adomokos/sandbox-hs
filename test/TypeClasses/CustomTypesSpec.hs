module TypeClasses.CustomTypesSpec where

import Test.Hspec

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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TypeClass Custom Implementations" $ do
    context "Maybe-like MyMaybe" $ do
      it "works as Functor" $ do
        fmap (+2) MyNothing `shouldBe` MyNothing
        fmap (+2) (MyJust 3) `shouldBe` MyJust 5
      it "works as Applicative" $ do
        (+) <$> (MyJust 2) <*> (MyJust 3)
          `shouldBe` MyJust 5
        (+) <$> MyNothing <*> (MyJust 3)
          `shouldBe` MyNothing
      it "works as Monad" $ do
        (pure 2 >>= (\x -> MyJust (x+3))) `shouldBe` MyJust 5
        (MyNothing >>= (\x -> MyJust (x+3))) `shouldBe` MyNothing
        (pure 2 >>= (\_ -> MyNothing)) `shouldBe` (MyNothing :: MyMaybe Int)

    context "Either-like MyEither" $ do
      it "works as Functor" $ do
        fmap (+2) (MyLeft "Hello")
          `shouldBe` (MyLeft "Hello" :: MyEither String Int)
        fmap (+2) (MyRight 5) `shouldBe` (MyRight 7 :: MyEither String Int)
      it "works as Aplicative" $ do
        MyRight (+2) <*> MyRight 3
          `shouldBe` (MyRight 5 :: MyEither String Int)
        MyRight (+2) <*> MyLeft "Hello"
          `shouldBe` (MyLeft "Hello" :: MyEither String Int)
        MyLeft "Hello" <*> MyRight 5
          `shouldBe` (MyLeft "Hello" :: MyEither String Int)
      it "works as Monad" $ do
        (MyLeft "Hello" >>= (\x -> MyRight (x+3)))
          `shouldBe` MyLeft "Hello"
        (pure 2 >>= (\x -> MyRight (x+3)))
          `shouldBe` (MyRight 5 :: MyEither String Int)
        (pure 2 >>= (\_ -> MyLeft "Hello"))
          `shouldBe` (MyLeft "Hello" :: MyEither String Int)
