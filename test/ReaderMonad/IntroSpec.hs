{-# LANGUAGE InstanceSigs #-}
module ReaderMonad.IntroSpec where

import Test.Hspec
import Control.Monad (liftM, liftM2)

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure x = Reader $ \_e -> x
  Reader f <*> Reader x = Reader $ \e -> (f e) (x e)

instance Monad (Reader r) where
  return = pure
  x >>= f = Reader $ \e -> runReader (f (runReader x e)) e

ask :: Reader a a
ask = Reader id

asks :: (e -> a) -> Reader e a
-- asks f = do
  -- e <- ask
  -- pure $ f e
  -- more elegantly:
asks f = fmap f ask

-- local transforms the environment the reader sees
local :: (e -> t) -> Reader t a -> Reader e a
-- local f r = do
  -- e <- ask
  -- pure $ runReader r (f e)
  -- more elegantly:
local f r = fmap (\e -> runReader r (f e)) ask

fnReader :: a -> Reader e (a, e)
fnReader x = do
  e <- ask
  pure $ (x, e)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Reader Monad" $ do
    it "works as Functor" $ do
      let readerApp = fmap (+2) (Reader (*3))
      runReader readerApp 3 `shouldBe` 11
    it "works as Applicative" $ do
      let readerApp = (pure (+2)) <*> (Reader (*3))
      runReader readerApp 3 `shouldBe` 11
    it "works as monad" $ do
      let readerApp = Reader (+2) >>= \x -> Reader (*x)
      runReader readerApp 3 `shouldBe` 15

  describe "in action" $ do
    it "works with ask, extracting info from environment" $ do
      let x = runReader (fnReader 10) 20
      x `shouldBe` (10,20)
    it "works with asks" $ do
      let reader = Reader (*3)
      (runReader $ local (+2) reader) 5 `shouldBe` 21
      -- pending
    it "works with different examples" $ do
      let x = runReader (pure 10 >>= fnReader) 20
      x `shouldBe` (10,20)
      let y = runReader (ask >>= fnReader) 20
      y `shouldBe` (20,20)
      let z = runReader (liftM (*10) ask) 20
      z `shouldBe` 200
      let v = runReader (liftM2 (,) ask ask) 20
      v `shouldBe` (20,20)
