module Calc where

import ExprT
import Parser

-- Ex01

-- be able to eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Ex02

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

-- Ex03

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Ex04

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = lit $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


-- Ex05 (or Ex06)
