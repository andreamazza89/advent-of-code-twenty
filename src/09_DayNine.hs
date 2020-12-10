module DayNine where

import Data.List

solutions :: IO ()
solutions =
  readFile "./data/09.data"
    >>= print . solveTwo 36845998 [] 1 . map read . lines

solveOne :: Int -> [Int] -> Int
solveOne windowSize numbers
  | numberToCheck `elem` sums = solveOne windowSize (tail numbers)
  | otherwise = numberToCheck
  where
    numberToCheck = numbers !! windowSize
    sums = [x + y | x <- numbers, y <- numbers]

solveTwo :: Int -> [Int] -> Int -> [Int] -> Int
solveTwo targetNumber subGroup subGroupSize numbers
  | sum subGroup == targetNumber = Data.List.minimum subGroup + Data.List.maximum subGroup
  | sum subGroup > targetNumber = solveTwo targetNumber [] 1 (tail numbers)
  | otherwise = solveTwo targetNumber (take subGroupSize numbers) (subGroupSize + 1) numbers
