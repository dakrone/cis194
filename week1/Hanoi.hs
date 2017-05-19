{-# OPTIONS_GHC -Wall #-}

-- The towers of Hanoi
module Hanoi where

type Peg = String

type Move = (Peg, Peg)

-- Given the number of discs and names for the three pegs, return a list of
-- moves to be performed to move the stack of discs from the first peg to the
-- second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) -- move from a -> c using b
  ++ [(a, b)] -- move from a -> b
  ++ (hanoi (n - 1) c b a) -- move from c -> b using a
