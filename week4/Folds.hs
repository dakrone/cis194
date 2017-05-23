module Folds where

import Data.List (sort)

-- Ex01

-- hint: iterate and takeWhile

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- Same thing, but implemented differently
fun1' :: [Integer] -> Integer
fun1' x = foldl (\a b -> (b - 2) * a) 1 $ filter even x

fun2' :: Integer -> Integer
fun2' = undefined


-- Ex02

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- generate a balanced binary tree from a list of values using foldr
foldTree :: Ord a => [a] -> Tree a
foldTree a =
  let sa = sort a
  in Leaf


-- Ex03

-- implement xor for a list of bools using a fold
xor :: [Bool] -> Bool
xor bs = foldl xor' False $ filter (== True) bs
  where xor'= \a b -> if a && b then False else a || b

-- implement 'map' as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> (f a) : b) []


-- Ex04

cardProd :: [a] -> [b] -> [(a, b)]
cardProd xs ys = [(x,y) | x <- xs, y <- ys]


-- implement sieve of sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined
