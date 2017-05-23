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
