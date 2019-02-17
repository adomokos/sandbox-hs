{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module StateMonad.BankExampleSpec where

import Control.Monad (ap, liftM)
-- https://medium.com/@Gryff/bank-kata-in-haskell-dealing-with-state-3364c13b994f
import Test.Hspec

main :: IO ()
main = hspec spec

data Transaction
  = Deposit Int
  | Withdrawal Int
  deriving (Eq)

instance Show Transaction where
  show (Deposit x) = "Deposited " ++ show x
  show (Withdrawal x) = "Withdrew " ++ show x

deposit :: Int -> [Transaction] -> [Transaction]
deposit amount transactions = transactions ++ [Deposit amount]

withdraw :: Int -> [Transaction] -> [Transaction]
withdraw amount transactions = transactions ++ [Withdrawal amount]

getStatement :: [Transaction] -> (String, [Transaction])
getStatement =
  foldr
    (\txn (statements, txns') -> (logTxn txn statements, txn : txns'))
    ("", [])
  where
    logTxn txn statements' = (show txn <> "\n") <> statements'

useMyBank :: [Transaction] -> (String, [Transaction])
useMyBank initialTransactions =
  let newTransactions = deposit 100 initialTransactions
      newTransactions2 = withdraw 50 newTransactions
      (statement, newTransactions3) = getStatement newTransactions2
   in (statement, newTransactions3)

-- Refactoring to State monad
-- 1. match the signature of deposit and withdraw
deposit' :: Int -> [Transaction] -> ((), [Transaction])
deposit' amount transactions = ((), transactions ++ [Deposit amount])

withdraw' :: Int -> [Transaction] -> ((), [Transaction])
withdraw' amount transactions = ((), transactions ++ [Withdrawal amount])

useMyBank' :: [Transaction] -> (String, [Transaction])
useMyBank' initialTransactions =
  let ((), newTransactions) = deposit' 100 initialTransactions
      ((), newTransactions2) = withdraw' 50 newTransactions
      (statement, newTransactions3) = getStatement newTransactions2
   in (statement, newTransactions3)

-- 2. Use the state monad
newtype State s a = State
  { runState :: s -> (a, s)
  }

-- more concretely for our use case:
-- State [Transaction] a = { runState :: [Transaction] -> (a, [Transaction]) }
instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State (x, )
  (State h) >>= f =
    State $ \s ->
      let (a, newState) = h s
          (State g) = f a
       in g newState

evalState :: State s a -> s -> a
evalState f = fst . runState f

execState :: State s a -> s -> s
execState f = snd . runState f

modify :: ([Transaction] -> [Transaction]) -> State [Transaction] ()
modify f = State (\transactions -> ((), f transactions))

gets :: ([Transaction] -> String) -> State [Transaction] String
gets f = State (\transactions -> (f transactions, transactions))

depositSt :: Int -> State [Transaction] ()
depositSt amount =
  State (\transactions -> ((), transactions ++ [Deposit amount]))

depositSt' :: Int -> State [Transaction] ()
depositSt' amount = modify (\transactions -> transactions ++ [Deposit amount])

withdrawSt :: Int -> State [Transaction] ()
withdrawSt amount =
  State (\transactions -> ((), transactions ++ [Withdrawal amount]))

generateStatement :: [Transaction] -> String
generateStatement = concatMap (\x -> show x <> "\n")

getStatementSt :: State [Transaction] String
-- getStatementSt = State (\transactions -> (generateStatement transactions, transactions))
getStatementSt = gets generateStatement

useMyBankSt :: State [Transaction] String
useMyBankSt = do
  depositSt' 200
  withdrawSt 100
  getStatementSt

spec :: Spec
spec = do
  describe "Bank transactions with State Monad" $ do
    it "works without it" $ do
      let initial = deposit 100 []
      initial `shouldBe` [Deposit 100]
      let withdrawal' = withdraw 50 initial
      withdrawal' `shouldBe` [Deposit 100, Withdrawal 50]

    it "can generate a statement" $ do
      let (statements, _txns) = useMyBank []
      statements `shouldBe` "Deposited 100\nWithdrew 50\n"

  describe "transitioning over to State Monad" $
    it "still works similarly" $ do
      let (statements, _txns) = useMyBank' []
      statements `shouldBe` "Deposited 100\nWithdrew 50\n"

  describe "Use state monad" $ do
    it "generates the proper statement" $ do
      execState (depositSt 100) [] `shouldBe` [Deposit 100]
      evalState getStatementSt [Withdrawal 100] `shouldBe` "Withdrew 100\n"

    it "uses useMyBankSt" $ do
      let statement = evalState useMyBankSt []
      statement `shouldBe` "Deposited 200\nWithdrew 100\n"
