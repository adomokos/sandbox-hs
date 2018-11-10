module Main where

import Lib
import ErrorReporting

main :: IO ()
main = do
  putStrLn "Program starting..."
  example1 1 0
  example2 1 0
  example2' 1 2 0
  example2' 1 2 3
  example3 1 0
  example3 1 2
  example3' 1 2 3
  example3' 1 2 0
  example4a 1 2
  example4a 1 0
  example4b 1 2
  putStrLn "ok"
  example4b 1 2
  example4c 1 2
  -- example4c 1 0
  let (Right x) = example5 5 2
  putStrLn x
  let (Right y) = example5 5 0
  putStrLn (show y)
  example7 4 2
  -- example7 4 0 -- this is not really catching an error
  example8 6 2
  example8 6 0
  reportResult (calculateLength "hello")
  reportResult (calculateLength "hello, world!")
  reportResult (calculateLength [])

