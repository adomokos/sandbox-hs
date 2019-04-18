module Async.WarmUp where

-- https://haskell.fpcomplete.com/library/async

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.Foldable (traverse_)

action1 :: IO Int
action1 = do
  threadDelay 500000
  pure 5

action2 :: IO String
action2 = do
  threadDelay 1000000
  pure "action2 result"

run :: IO ()
run = do
  -- `concurrently` function waits until both operations complete
  res <- concurrently action1 action2
  print (res :: (Int, String))

run1 :: IO ()
run1 = do
  res <- race action1 action2
  print (res :: Either Int String)

run2 :: IO ()
run2 = do
  res1 <-
    runConcurrently $ (,) <$> Concurrently action1 <*> Concurrently action2
  print (res1 :: (Int, String))

  res2 <-
    runConcurrently
    $   (Left <$> Concurrently action1)
    <|> (Right <$> Concurrently action2)
  print (res2 :: Either Int String)

-- Example to write file concurrently

type Score = Int
data Person = Person FilePath Score

people :: [Person]
people = [Person "alice.txt" 50, Person "bob.txt" 60, Person "charlie.txt" 70]

-- | This function returns a unit value that we don't care about. Using
-- concurrently on two such actions would give us ((), ()).
writePerson :: Person -> IO ()
writePerson (Person fp score) =
  -- writeFile fp (show score)
  putStrLn ("Writing file: " ++ show fp ++ " with " ++ show score)

writePeople :: [Person] -> IO ()
writePeople = runConcurrently . traverse_ (Concurrently . writePerson)

run3 :: IO ()
run3 = writePeople people

-- Example with throwing an exception
-- When one thread dies, the other one dies as well.
action1' :: IO Int
action1' = error "action1 errored"

action2' :: IO String
action2' = handle onErr $ do
  threadDelay 500000
  pure "action2 completed"
 where
  onErr e = do
    putStrLn $ "action2 was killed by: " ++ displayException e
    throwIO (e :: SomeException)

run4 :: IO ()
run4 = do
  res <- concurrently action1' action2'
  print res

-- Use race to continue running as long as the main thread is running

-- | Print successive numbers to stdout. Notice how it returns @a@ instead of
-- @()@. This lets the type system know that, under normal circumstances, this
-- function will never exit.
counter :: IO a
counter =
  let loop i = do
        putStrLn $ "counter: " ++ show i
        threadDelay 1000000
        loop $! i + 1
  in  loop 1

-- | This function will continue to run counter with whatever action you've
-- provided, and stop running counter once that action exists. If by some chance counter
-- throws an exception, it will take down your thread as well.
withCounter :: IO a -> IO a
withCounter inner = do
  res <- race counter inner
  case res of
    Left  x -> assert False x
    Right x -> pure x

run5 :: IO ()
run5 = do
  putStrLn "Before withCounter"
  threadDelay 2000000
  withCounter $ do
    threadDelay 2000000
    putStrLn "Inside withCounter"
    threadDelay 2000000
  threadDelay 2000000
  putStrLn "After withCounter"
  threadDelay 2000000
  putStrLn "Exiting!"
