{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.Set (toList, fromList)
import Data.List (sort)

-- Exercise 1

-- Return every @x@ elements in the array
every :: Int -> [a] -> [a]
every _ [] = []
every x yz =
  let i = zip [1..] yz
      filtered = filter (\t -> (fst t) `mod` x == 0) i
  in map snd filtered

-- First list should be the same as the input list, the second should contain
-- every second element from the input list, with the nth list having every nth
-- element
skips :: [a] -> [[a]]
skips l =
  -- Create a `length` repeat of the list, with indices
  let wo = zip [1..(length l)] (repeat l)
      -- map the "every" method over it
      we = map (\t -> every (fst t) (snd t)) wo
  in we

------------------------------

-- Exercise 2

-- Takes a list of numbers and returns a list that is the local maxima subset
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:zs)
  | x < y && y > z = y : (localMaxima $ y:z:zs)
  | otherwise = localMaxima $ y:z:zs

------------------------------

-- Exercise 3

-- Returns a (sorted) list of tuples representing the num and occurances
genNums :: [Integer] -> [(Integer, Int)]
genNums n = toList . fromList $ map (\i -> (i, length $ filter (\k -> k == i) n)) $ sort n

-- takes list of nums between 0 and 9 and outputs vertical histogram for each number
histogram :: [Integer] -> String
histogram = undefined
