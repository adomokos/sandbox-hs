{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module FoldableTraversable.EnhancedProjectSpec where

-- https://www.youtube.com/watch?v=pe6S5skZwNE

import Data.Foldable (fold)
import Data.Monoid (getSum)
import qualified Data.Text as T
import Data.Tree (Tree(..), drawTree)
import System.Random (getStdRandom, randomR)
import Test.Hspec
import Text.Printf (printf)

main :: IO ()
main = hspec spec

newtype Money = Money
  { unMoney :: Double }
  deriving (Show, Eq, Num)

newtype ProjectId = ProjectId
  { unProjectId :: Int }
  deriving (Show, Eq, Num)

data Project a
  = Project T.Text a
  | ProjectGroup T.Text [Project a]
  -- deriving (Show, Eq, Functor, Foldable, Traversable)
   deriving (Show, Eq)

instance Functor Project where
  fmap f (Project text x) = Project text (f x)
  fmap f (ProjectGroup text projects) = ProjectGroup text ((fmap . fmap) f projects)

instance Foldable Project where
  -- foldr f z (Project text x) = f x z
  -- foldl f z (Project text x) = f z x
  foldMap :: Monoid m => (a -> m) -> Project a -> m
  foldMap f = mconcat . map f . toAs

instance Traversable Project where
  traverse :: Applicative f => (a -> f b) -> Project a -> f (Project b)
  traverse f (Project text a) = Project text <$> f a
  traverse f (ProjectGroup text projs) =
      ProjectGroup text <$> traverse (traverse f) projs

toAs :: Project a -> [a]
toAs (Project _text a) = [a]
toAs (ProjectGroup _text projs) = concatMap toAs projs

data Budget = Budget
  { budgetIncome :: Money
  , budgetExpenditure :: Money
  } deriving (Show, Eq)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Show, Eq)

someProject :: Project ProjectId
someProject = ProjectGroup "Sweden" [stockholm, gothenburg, malmo]
 where
  stockholm = Project "Stockholm" 1
  gothenburg = Project "Gothenburg" 2
  malmo = ProjectGroup "Malmo" [city, limhamn]
  city = Project "Malmo City" 3
  limhamn = Project "Limhamn" 4

-- The DB queries
fetchBudget :: ProjectId -> IO Budget
fetchBudget _projectId = do
  income <- Money <$> getStdRandom (randomR (5000, 10000))
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
  , netProfit = netProfit'
  , difference = netProfit' - budgetProfit'
  }
 where
  budgetProfit' = budgetIncome budget - budgetExpenditure budget
  netProfit' = getSum (foldMap asProfit transactions)
  asProfit (Sale m) = pure m
  asProfit (Purchase m) = pure (negate m)

calculateProjectReport :: Project ProjectId -> IO (Project Report)
calculateProjectReport =
  traverse (\p -> calculateReport <$> fetchBudget p <*> fetchTransactions p)

accumulateProjectReport :: Project Report -> Report
accumulateProjectReport = fold

-- Pretty print
asTree :: (a -> String) -> Project a -> Tree String
asTree prettyValue project = case project of
  Project name x -> Node (printf "%s: %s" name (prettyValue x)) []
  ProjectGroup name projects ->
    Node (T.unpack name) (map (asTree prettyValue) projects)

prettyProject :: (a -> String) -> Project a -> String
prettyProject prettyValue = drawTree . asTree prettyValue

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
      length report `shouldBe` 4

      let singleReport = accumulateProjectReport report
      unMoney (budgetProfit singleReport) `shouldSatisfy` (> 0)
      unMoney (netProfit singleReport) `shouldSatisfy` (> 0)
      unMoney (difference singleReport) `shouldSatisfy` (< 0)
