module DayThree where

import Debug.Trace

type Square = Char

newtype MoveRight = MoveRight Int

data RowsToCheck = AllRows | EvenRows

solutions :: IO ()
solutions =
  linesAsMap "./data/03.data"
    >>= (print . solveTwo)

linesAsMap :: String -> IO [[Square]]
linesAsMap filepath =
  map (concat . repeat) . lines <$> readFile filepath

solveOne :: [[Square]] -> Int
solveOne = solveOne_ (MoveRight 3, AllRows)

solveOne_ :: (MoveRight, RowsToCheck) -> [[Square]] -> Int
solveOne_ instructions = (\(x, _, _) -> x) . foldl (checkSquare instructions) (0, 0, 0)

solveTwo :: [[Square]] -> Int
solveTwo treesMap =
  product
    [ solveOne_ (MoveRight 1, AllRows) treesMap,
      solveOne_ (MoveRight 3, AllRows) treesMap,
      solveOne_ (MoveRight 5, AllRows) treesMap,
      solveOne_ (MoveRight 7, AllRows) treesMap,
      solveOne_ (MoveRight 1, EvenRows) treesMap
    ]

checkSquare :: (MoveRight, RowsToCheck) -> (Int, Int, Int) -> [Square] -> (Int, Int, Int)
checkSquare (MoveRight toTheRight, rowsToCheck) (hits, columnNumber, rowNumber) treesRow =
  if shouldCheckRow rowsToCheck rowNumber
    then
      if treesRow !! columnNumber == '#'
        then (hits + 1, columnNumber + toTheRight, newRow)
        else (hits, columnNumber + toTheRight, newRow)
    else (hits, columnNumber, newRow)
  where
    newRow = rowNumber + 1

shouldCheckRow :: RowsToCheck -> Int -> Bool
shouldCheckRow rowsToCheck rowNumber =
  case rowsToCheck of
    AllRows -> True
    EvenRows -> even rowNumber
