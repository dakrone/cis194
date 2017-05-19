{-# OPTIONS_GHC -Wall #-}

-- A credit card validator
module CCValidator where

-- Converts a single integer into an array of its digits
toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0    = []
  | otherwise = toDigits (div i 10) ++ [i `mod` 10]

-- Converts a single integer into an array of digits reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Doubles every second number, starting from the left-hand side
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:zs) = [x, (y * 2)] ++ doubleEveryOtherFromLeft zs

-- Doubles every second number, starting from the right-hand side
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther nums = reverse $ doubleEveryOtherFromLeft $ reverse nums

-- Sum of each of the *digits* in the list of integers
sumDigits :: [Integer] -> Integer
sumDigits nums = sum $ map sum $ map toDigits nums

-- Validates the credit card number
validate :: Integer -> Bool
validate x = (sumDigits $  doubleEveryOther $ toDigits x) `mod` 10 == 0
