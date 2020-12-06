module DaySix where

import Data.List

solutions :: IO ()
solutions =
  readFile "./data/06.data"
    >>= print . solveTwo

solveOne :: String -> Int
solveOne =
  sum
    . map (length . filter (/= '_') . unique)
    . lines
    . removeExtraReturns ""

solveTwo :: String -> Int
solveTwo =
  sum
    . map (countAnswers . splitOn (== '_'))
    . lines
    . removeExtraReturns ""

countAnswers :: [String] -> Int
countAnswers [person] = length person
countAnswers (person : otherPeople) =
  foldr (countChar otherPeople) 0 person

countChar :: [String] -> Char -> Int -> Int
countChar otherPeople answer total =
  if all (elem answer) otherPeople
    then total + 1
    else total

removeExtraReturns :: String -> String -> String
removeExtraReturns output ('\n' : '\n' : rest) = removeExtraReturns ('\n' : output) rest
removeExtraReturns output ('\n' : rest) = removeExtraReturns ('_' : output) rest
removeExtraReturns output (char : rest) = removeExtraReturns (char : output) rest
removeExtraReturns output [] = reverse output

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn shouldSplit [] = []
splitOn shouldSplit toSplit =
  leftChunk : splitOn shouldSplit (safeTail otherChunks)
  where
    (leftChunk, otherChunks) = break shouldSplit toSplit

unique = Data.List.nub

safeTail (x : xs) = xs
safeTail [] = []
