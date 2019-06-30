{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
module RIO.IntroSpec where

-- examples from here: https://haskell.fpcomplete.com/library/rio

import Test.Hspec

import RIO

type Name = String
type MyHandle = (String -> String)

main :: IO ()
main = hspec spec

-- Example 1, simple type alias as underlying env Value

run1 :: IO (String, String)
run1 = do
  let name = "Alice"
  runRIO name $ do
    hello <- sayHello
    bye   <- sayGoodBye
    pure (hello, bye)

sayHello :: RIO Name String
sayHello = do
  name <- ask
  let result = "Hello, " ++ name
  -- liftIO . putStrLn $ result - Use liftIO to run underlying IO actions
  pure result

sayGoodBye :: RIO Name String
sayGoodBye = do
  name <- ask
  pure $ "Bye, " ++ name

-- Example 2, use a more complext type for env value

data App = App
  { appName :: !String
  , appHandle :: !MyHandle
  }

run2 :: IO (String, String)
run2 = do
  let app = App {appName = "Alice", appHandle = (++ " handled!")}
  runRIO app $ do
    hello <- sayHello2
    bye   <- sayGoodBye2
    pure (hello, bye)

sayHello2 :: RIO App String
sayHello2 = do
  App name h <- ask
  let result = "Hello, " ++ name
  pure . h $ result

sayGoodBye2 :: RIO App String
sayGoodBye2 = do
  App name h <- ask
  pure . h $ "Bye, " ++ name

-- Example 3, demonstrating Has* typeclasses

run3 :: IO (String, String, String)
run3 = do
  let app = App {appName = "Alice", appHandle = (++ " handled!")}
  runRIO app $ do
    hello <- sayHello3
    bye   <- sayGoodBye3
    time  <- sayTime
    pure (hello, bye, time)

say :: String -> RIO App String
say msg = do
  App _name h <- ask
  pure . h $ " >saying< " <> msg

sayHello3 :: RIO App String
sayHello3 = do
  App name _h <- ask
  let result = "Hello, " ++ name
  say result

sayGoodBye3 :: RIO App String
sayGoodBye3 = do
  App name _h <- ask
  say $ "Bye, " ++ name

-- The `App` dependency makes this function too coupled
sayTime :: (HasHandle env) => RIO env String
sayTime = do
  let now = "2019/04/01" -- this should come from `getCurrentTime`
  sayH $ "The date is: " ++ now

class HasHandle env where
  getHandle :: env -> MyHandle

instance HasHandle MyHandle where
  getHandle = id
instance HasHandle App where
  getHandle = appHandle

-- Not how the type restriction is loosen up
sayH :: (HasHandle env) => String -> RIO env String
sayH msg = do
  env <- ask
  pure . getHandle env $ " >saying< " <> msg

-- Example 4, lens for env

run4 :: IO (String, String)
run4 = do
  let app = App {appName = "Alice", appHandle = (++ " handled!")}
  runRIO app $ do
    hello <- switchHandle (++ " handled with new!") sayHello3
    time  <- sayTime
    pure (hello, time)

-- :-( Sadly, the App type has to specified in the type
switchHandle :: MyHandle -> RIO App a -> RIO App a
switchHandle h inner = do
  app <- ask
  let app' = app { appHandle = h }
  runRIO app' inner

-- Introducing lenses, we can loosen up this restriction
class HasHandle2 env where
  handleL :: Lens' env MyHandle

instance HasHandle2 App where
  handleL = lens appHandle (\x y -> x { appHandle = y })

-- With this lens implementation, the constraints get looser
switchHandle2 :: (HasHandle2 env) => MyHandle -> RIO env a -> RIO env a
switchHandle2 h inner = do
  env <- ask
  let env' = set handleL h env
  runRIO env' inner

run4b :: IO (String, String)
run4b = do
  let app = App {appName = "Alice", appHandle = (++ " handled!")}
  runRIO app $ do
    hello <- switchHandle2 (++ " handled with new!") sayHello3
    time  <- sayTime
    pure (hello, time)

spec :: Spec
spec = do
  describe "Intro to RIO" $ do
    it "works with simple type in environment" $ do
      run1 `shouldReturn` ("Hello, Alice", "Bye, Alice")
    it "works with a record type in environment" $ do
      run2 `shouldReturn` ("Hello, Alice handled!", "Bye, Alice handled!")
    it "works with Has* typeclasses" $ do
      run3
        `shouldReturn` ( " >saying< Hello, Alice handled!"
                       , " >saying< Bye, Alice handled!"
                       , " >saying< The date is: 2019/04/01 handled!"
                       )
    it "can swap handlers mid-flight" $ do
      run4
        `shouldReturn` ( " >saying< Hello, Alice handled with new!"
                       , " >saying< The date is: 2019/04/01 handled!"
                       )
      run4b
        `shouldReturn` ( " >saying< Hello, Alice handled with new!"
                       , " >saying< The date is: 2019/04/01 handled!"
                       )
