{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ReaderMonad.ReaderTSpec where

-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

import Test.Hspec

import Control.Concurrent.Async.Lifted
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent.STM
import Say
import Prelude hiding (log)

main :: IO ()
main = hspec spec

stateExample :: IO Int
stateExample =
  execStateT
    (concurrently (modify (+1)) (modify (+2)))
    4

modifyTVarRef :: (MonadReader (TVar Int) m, MonadIO m)
              => (Int -> Int)
              -> m ()
modifyTVarRef f = do
  ref <- ask
  liftIO $ atomically $ modifyTVar' ref f

tVarReaderT :: IO Int
tVarReaderT = do
  ref <- newTVarIO 4
  _ <- runReaderT (concurrently (modifyTVarRef (+1)) (modifyTVarRef (+2))) ref
  readTVarIO ref

-- Has typeclass approach
data Env = Env {
    envLog :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

modifyIt :: (MonadReader Env m, MonadIO m)
         => (Int -> Int)
         -> m ()
modifyIt f = do
  env <- ask
  liftIO $ atomically $ modifyTVar' (envBalance env) f

logSomething :: (MonadReader Env m, MonadIO m)
             => String
             -> m ()
logSomething _msg = do
  _env <- ask
  -- liftIO $ envLog env msg
  pure ()

-- Use Has typeclass trick

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasBalance a where
  getBalance :: a -> TVar Int
instance HasBalance (TVar Int) where
  getBalance = id
instance HasBalance Env where
  getBalance = envBalance

modifyIt' :: (MonadReader env m, HasBalance env, MonadIO m)
          => (Int -> Int)
          -> m ()
modifyIt' f = do
  env <- ask
  liftIO $ atomically $ modifyTVar' (getBalance env) f

logSomething' :: (MonadReader env m, HasLog env, MonadIO m)
              => String
              -> m ()
logSomething' _msg = do
  _env <- ask
  -- liftIO $ getLog env msg
  pure ()

logSomething'' :: (MonadReader env m, HasLog env, MonadIO m)
               => String
               -> m ()
logSomething'' msg = do
  env <- ask
  liftIO $ getLog env msg
  pure ()

spec :: SpecWith ()
spec = do
  describe "ReaderT Example" $ do
    it "StateT does not fix mutable state, hides it" $
      stateExample `shouldReturn` 6
    it "works better with TVar" $
     tVarReaderT `shouldReturn` 7

  describe "Env with logging function" $ do
    it "can use TVar" $ do
      ref <- newTVarIO 4
      let env = Env {
                envLog = sayString
              , envBalance = ref
              }
      _ <- runReaderT
        (concurrently
          (modifyIt (+1))
          (logSomething "Increasing account balance")
        )
        env
      balance <- readTVarIO ref
      balance `shouldBe` 5
    it "can use the Has typeclass trick" $ do
      ref <- newTVarIO 4
      let env = Env {
                envLog = sayString
              , envBalance = ref
              }
      _ <- runReaderT
        (concurrently
          (modifyIt' (+1))
          (logSomething' "Increasing account balance")
          -- (logSomething' "")
        )
        env
      balance <- readTVarIO ref
      balance `shouldBe` 5
    describe "Unit testing" $ do
      it "can modify" $ do
        var <- newTVarIO (1 :: Int)
        _ <- runReaderT (modifyIt' (+ 2)) var
        res <- readTVarIO var
        res `shouldBe` 3
      it "can log" $ do
        var <- newTVarIO ""
        let logFunc msg = atomically $ modifyTVar var (++ msg)
            msg1 = "Hello "
            msg2 = "World\n"
        runReaderT (logSomething'' msg1 >> logSomething'' msg2) logFunc
        res <- readTVarIO var
        res `shouldBe` msg1 ++ msg2
