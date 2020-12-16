module DayThirteen where

import Data.List

solveOne :: [(Int, Int)] -- did some of this manually (found the closest and multiplied by the number of wait minutes)
solveOne =
  zip [17, 41, 937, 13, 23, 29, 397, 37, 19]
    . map (findNearest 1007268 . generateTimetable)
    $ [17, 41, 937, 13, 23, 29, 397, 37, 19]

findNearest :: Int -> [Int] -> Int
findNearest target (x : y : rest)
  | target >= x && target <= y = y
  | otherwise = findNearest target (y : rest)

generateTimetable :: Int -> [Int]
generateTimetable busId =
  map (* busId) [1 ..]
