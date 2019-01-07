module Main where

import Options.Applicative as Opt
import Data.Semigroup ( (<>) )
import Data.Char (toUpper)

{-
  Types:
  1. Command
  2. Switch
  3. Args
-}

data Welcome = Welcome { name :: String
                       , excited :: Bool }

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn $ transform $
    "Enjoy the snow, " ++ name opts ++ "!"
  where
    transform = if excited opts then map toUpper else id

main :: IO ()
main = execParser opts >>= runWithOptions
  -- execParser :: ParserInfo a -> IO a
    where
      parser = Welcome <$> argument str (metavar "NAME")
                       <*> switch (short 'e' <>
                                   long "excited" <>
                                    help "Run in excited mode.")
      opts = info parser mempty
      -- info :: Parser a -> InfoMod a -> ParserInfo a

