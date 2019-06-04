{-# LANGUAGE GADTs #-}
module GADTs.IntroSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data Expr = I Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show, Eq)

eval :: Expr -> Int
eval (I n      ) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Stir it up a bit with adding Bool
data Exp = I' Int
         | B Bool
         | Adding Exp Exp
         | Multiply Exp Exp
         | Eq Exp Exp

testExp :: Exp
testExp = (I' 5 `Adding` I' 1) `Eq` I' 7

-- evalExp can have the value of Int or Bool
{-
evalExp :: Exp -> Either Int Bool
evalExp (I' nVal) = Left nVal
evalExp (B  bVal) = Right bVal
-}
-- evalExp (Add e1 e2) = eval e1 + eval e2 -- this is illegal, e1 can be Bool or Int

-- We want more type safety!

-- Phantom types...
-- a is the dummy or phantom type, there is no value
-- of a type a "inside" MyExp a
data MyExp a = MyInt Int
             | MyBool Bool
             | MyAdd (MyExp a) (MyExp a)
             | MyMul (MyExp a) (MyExp a)
             | MyEq (MyExp a) (MyExp a)
             deriving (Show, Eq)

-- Add :: MyExp a -> MyExp a -> MyExp a

b :: Bool -> MyExp Bool
b val = MyBool val

i :: Int -> MyExp Int
i val = MyInt val

-- Int now is restricted type
add :: MyExp Int -> MyExp Int -> MyExp Int
add = MyAdd

-- eq :: MyExp Int -> MyExp Int -> MyExp Bool
-- eq = MyEq

{-
myEval :: MyExp a -> a
myEval (MyInt n) = n
        -- MyInt :: MyExp a and a can be anything (a -> String?)
-}

-- GADTs!
data GExpr a where
  GInt ::  Int -> GExpr Int
  GBool :: Bool -> GExpr Bool
  GAdd :: GExpr Int -> GExpr Int -> GExpr Int
  GMul :: GExpr Int -> GExpr Int -> GExpr Int
  GEq :: GExpr Int -> GExpr Int -> GExpr Bool

gEval :: GExpr a -> a
gEval (GInt  n'  ) = n'
gEval (GBool b'  ) = b'
gEval (GAdd e1 e2) = gEval e1 + gEval e2
gEval (GMul e1 e2) = gEval e1 * gEval e2
gEval (GEq  e1 e2) = gEval e1 == gEval e2

spec :: Spec
spec = do
  describe "GADTs Intro" $ do
    it "works as expressions" $ do
      eval ((I 5 `Add` I 1) `Mul` I 7) `shouldBe` 42
    it "is more restrictive with Phantom Types" $ do
      -- This one wouldn't even compile, it does not type check
      -- b True `add` b False `shouldBe` False
      i 10 `add` i 12 `shouldBe` MyAdd (MyInt 10) (MyInt 12)
    it "works without any problems with GADTs" $ do
      gEval (GInt 2 `GAdd` GInt 5) `shouldBe` 7
      gEval (((GInt 2 `GAdd` GInt 5) `GMul` GInt 3) `GEq` GInt 21)
        `shouldBe` True
