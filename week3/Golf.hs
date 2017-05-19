{-# OPTIONS_GHC -Wall #-}

module Golf where

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

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:zs)
  | x < y && y > z = y : (localMaxima $ y:z:zs)
  | otherwise = localMaxima $ y:z:zs

------------------------------

-- Exercise 3
