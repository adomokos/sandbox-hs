module Main where

import Options.Applicative as Opt
import Data.Semigroup ( (<>) )
import Data.Char

{-
  Types:
  1. Command
  2. Switch
  3. Args
-}

data Welcome = Welcome { name :: String }

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn ("Enjoy the snow, " ++ name opts ++ "!")

main :: IO ()
main = execParser opts >>= runWithOptions
  -- execParser :: ParserInfo a -> IO a
    where
      parser = Welcome <$> argument str (metavar "NAME")
      opts = info parser mempty
      -- info :: Parser a -> InfoMod a -> ParserInfo a

