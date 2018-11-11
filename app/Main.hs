module Main where

import qualified XmlLearning.Runner as XL
import qualified Errors.Runner as ER

main :: IO ()
main = do
  XL.run
  ER.run
