{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadTransformers.StackSpec where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Test.Hspec

main :: IO ()
main = hspec spec

newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
  deriving (Functor, Applicative, Monad)

fooStack :: Stack ()
fooStack = Stack $ do
  put 1 -- State layer
  lift $ tell [2] -- Writer layer
  -- Disabled it, no need to print it
  -- lift $ lift $ print 3 -- IO layer
  -- liftIO $ print 3 -- read the IO layer directly
  pure ()

evalStack :: Stack a -> IO [Int]
evalStack m = execWriterT (evalStateT (unStack m) 0)

spec :: Spec
spec = do
  describe "creates the stack" $ do
    it "is layered" $ do
      res <- evalStack fooStack
      res `shouldBe` [2]
