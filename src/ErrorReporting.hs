{-# LANGUAGE FlexibleContexts #-}
module ErrorReporting where

-- http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/

import qualified Control.Exception as E
-- import           Control.Monad.Error
import           Control.Monad.Except
import qualified Control.Monad.Trans.Except as TE
import           Data.Typeable

-- Different ways to report error

-- | 1. Use error
myDiv1 :: Float -> Float -> Float
myDiv1 x 0 = error "Division by zero"
myDiv1 x y = x / y

example1 :: Float -> Float -> IO ()
example1 x y =
  E.catch (putStrLn (show (myDiv1 x y)))
          (\err -> putStrLn (show (err :: E.ErrorCall)))

-- | 2. Use Maybe
myDiv2 :: Float -> Float -> Maybe Float
myDiv2 x 0 = Nothing
myDiv2 x y = Just (x/y)

example2 :: Float -> Float -> IO ()
example2 x y =
  case myDiv2 x y of
    Nothing -> putStrLn "Division by zero"
    Just q  -> putStrLn (show q)

-- And thanks to the magic of monads, we can actually string together calls to myDiv2
divSum2 :: Float -> Float -> Float -> Maybe Float
divSum2 x y z = do
  xdy <- myDiv2 x y
  xdz <- myDiv2 x z
  return (xdy + xdz)

example2' :: Float -> Float -> Float -> IO ()
example2' x y z =
  case divSum2 x y z of
    Nothing -> putStrLn "Division by zero"
    Just x  -> putStrLn (show x)

-- | Use Either String a
myDiv3 :: Float -> Float -> Either String Float
myDiv3 x 0 = Left "Division by zero"
myDiv3 x y = Right ( x / y )

example3 :: Float -> Float -> IO ()
example3 x y =
  case myDiv3 x y of
    Left msg -> putStrLn msg
    Right q  -> putStrLn (show q)

-- Either is a monad
divSum3 :: Float -> Float -> Float -> Either String Float
divSum3 x y z = do
  xdy <- myDiv3 x y
  xdz <- myDiv3 x z
  return (xdy + xdz)

example3' :: Float -> Float -> Float -> IO ()
example3' x y z =
  case divSum3 x y z of
    Left msg -> putStrLn msg
    Right r  -> putStrLn (show r)

-- | 4. Use Monad and fail to generalize 1-3
-- What if we don't care what monad our caller is using?
-- This error-reporting is widely used in standard libraries.
myDiv4 :: (Monad m) => Float -> Float -> m Float
myDiv4 x 0 = fail "Division by zero"
myDiv4 x y = return (x / y)

example4a x y =
  case myDiv4 x y of
    Nothing -> putStrLn "Divison by zero"
    Just q -> putStrLn (show q)

example4b x y =
  case myDiv4 x y of
    Left msg -> putStrLn msg
    Right q -> putStrLn (show q)

example4c x y =
  E.catch (do q <- myDiv4 x y
              putStrLn (show q))
          (\err -> putStrLn (show (err :: E.ErrorCall)))

-- | 5. Use `MonadError` and a custom error type
-- What if we want to keep track of specific errors?
data CustomError = DivByZero
                 | OutOfCheese
                 | MiscError String
  deriving (Typeable)

instance Show CustomError where
  show DivByZero       = "Division by zero"
  show OutOfCheese     = "Out of cheese"
  show (MiscError str) = str

-- This works like the fail example, but instead of using error messages
-- we user error values:
myDiv5 :: (MonadError CustomError m) =>
          Float -> Float -> m Float
myDiv5 x 0 = throwError DivByZero
myDiv5 x y = return (x / y)

example5 :: Float -> Float ->
            Either CustomError String
example5 x y =
  catchError (do q <- myDiv5 x y
                 return (show q))
             (\err -> return (show err))

-- | 6. Use ioerror and catch
myDiv7 :: Float -> Float -> IO Float
myDiv7 x 0 = ioError (userError "Division by zero")
myDiv7 x y = return (x / y)

-- example7 :: Float -> Float -> IO String
example7 x y =
  E.catch (do q <- myDiv7 x y
              putStrLn (show q))
          (\err -> putStrLn (show (err :: E.ErrorCall)))

-- | Go nuts with monad transformers
type ErrIO = TE.ExceptT String IO

myDiv8 :: Float -> Float -> ErrIO Float
myDiv8 x 0 = throwError "Division by zero"
myDiv8 x y = return (x / y)

example8 x y = do
  result <- TE.runExceptT (myDiv8 x y)
  case result of
    Left err -> putStrLn err
    Right q -> putStrLn (show q)

-- http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Error.html#g:2
data LengthError = EmptyString
                 | StringTooLong Int
                 | OtherError String

instance Show LengthError where
  show EmptyString = "The string was empty!"
  show (StringTooLong len) =
    "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
  show (OtherError msg) = msg

type LengthMonad = Either LengthError

calculateLength :: String -> LengthMonad Int
calculateLength s = (calculateLengthOrFail s) `catchError` Left

calculateLengthOrFail :: String -> LengthMonad Int
calcualteLengthOrFail [] = throwError EmptyString
calculateLengthOrFail s | len > 5   = throwError (StringTooLong len)
                        | otherwise = return len
  where len = length s

reportResult :: LengthMonad Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
