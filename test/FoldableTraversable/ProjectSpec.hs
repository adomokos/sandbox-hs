{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module FoldableTraversable.ProjectSpec where

-- https://www.youtube.com/watch?v=pe6S5skZwNE

import Data.Monoid (getSum)
import qualified Data.Text as T
import Data.Tree
import System.Random (getStdRandom, randomR)
import Test.Hspec
import Text.Printf

main :: IO ()
main = hspec spec

newtype Money =
  Money
  { unMoney :: Double }
  deriving (Show, Eq, Num)

newtype ProjectId =
  ProjectId
  { unProjectId :: Int }
  deriving (Show, Eq, Num)

data Project
  = Project ProjectId T.Text
  | ProjectGroup T.Text [Project]
  deriving (Show, Eq)

data Budget =
  Budget
    { budgetIncome :: Money
    , budgetExpenditure :: Money }
    deriving (Show, Eq)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Show, Eq)

someProject :: Project
someProject = ProjectGroup "Sweden" [stockholm, gothenburg, malmo]
 where
  stockholm  = Project 1 "Stockholm"
  gothenburg = Project 2 "Gothenburg"
  malmo      = ProjectGroup "Malmo" [city, limhamn]
  city       = Project 3 "Malmo City"
  limhamn    = Project 4 "Limhamn"

-- The DB queries
fetchBudget :: ProjectId -> IO Budget
fetchBudget _projectId = do
  income      <- Money <$> getStdRandom (randomR (5000, 10000))
  expenditure <- Money <$> getStdRandom (randomR (0, 5000))
  pure Budget {budgetIncome = income, budgetExpenditure = expenditure}

fetchTransactions :: ProjectId -> IO [Transaction]
fetchTransactions _projectId = do
  let sale     = Sale $ Money 438
      purchase = Purchase $ Money 354
  pure [sale, purchase]

-- Reporting
data Report = Report
  { budgetProfit :: Money
  , netProfit :: Money
  , difference :: Money
} deriving (Show, Eq)

instance Semigroup Report where
  (Report b1 n1 d1) <> (Report b2 n2 d2) =
    Report (b1+b2) (n1+n2) (d1+d2)

instance Monoid Report where
  mempty = Report 0 0 0

calculateReport :: Budget -> [Transaction] -> Report
calculateReport budget transactions = Report
  { budgetProfit = budgetProfit'
  , netProfit    = netProfit'
  , difference   = netProfit' - budgetProfit'
  }
 where
  budgetProfit' = budgetIncome budget - budgetExpenditure budget
  netProfit'    = getSum (foldMap asProfit transactions)
  asProfit (Sale     m) = pure m
  asProfit (Purchase m) = pure (negate m)

calculateProjectReport :: Project -> IO Report
calculateProjectReport = calc
 where
  calc (Project p _) =
    calculateReport <$> fetchBudget p <*> fetchTransactions p
  calc (ProjectGroup _ projects) = foldMap calc projects

-- Pretty print
asTree :: Project -> Tree String
asTree project = case project of
  Project (ProjectId p) name -> Node (printf "%s (%d)" name p) []
  ProjectGroup name projects -> Node (T.unpack name) (map asTree projects)

prettyProject :: Project -> String
prettyProject = drawTree . asTree

prettyReport :: Report -> String
prettyReport r = printf "Budget: %.2f, Net: %.2f, Difference: %+.2f"
                        (unMoney (budgetProfit r))
                        (unMoney (netProfit r))
                        (unMoney (difference r))

spec :: Spec
spec = do
  describe "Domain Modeling" $ do
    it "works with various types" $ do
      report <- calculateProjectReport someProject
      unMoney (budgetProfit report) `shouldSatisfy` (> 0)
      unMoney (netProfit report) `shouldSatisfy` (> 0)
      unMoney (difference report) `shouldSatisfy` (< 0)
