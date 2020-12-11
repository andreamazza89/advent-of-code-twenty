module DayEleven where

import Debug.Trace
import Control.Parallel.Strategies

data Cell
  = Floor
  | Seat Seat_
  deriving (Eq, Show)

data Seat_
  = Occupied
  | Empty
  deriving (Eq, Show)

data Location = Location
  { x :: Int,
    y :: Int
  }
  deriving (Eq, Show)

type Cells = [(Location, Cell)]

solutions :: IO ()
solutions =
  readFile "./data/11.data"
    >>= print . solveOne . linesAsCells


solveOne :: Cells -> Int
solveOne =
  length . filter isOccupied . solveOne_ []

solveOne_ :: Cells -> Cells -> Cells
solveOne_ cells cells'
  | cells == cells' = cells
  | otherwise =  solveOne_ cells' (tick cells')

tick :: Cells -> Cells
tick cells =
  parMap rseq (evolveCell cells) cells

evolveCell :: Cells -> (Location, Cell) -> (Location, Cell)
evolveCell cells cell@(location, Seat Empty) =
  if occupiedNearby cells location == 0
    then (location, Seat Occupied)
    else cell
evolveCell cells cell@(location, Seat Occupied) =
  if occupiedNearby cells location >= 4
    then (location, Seat Empty)
    else cell
evolveCell _ cell = cell

occupiedNearby :: Cells -> Location -> Int
occupiedNearby cells =
  length . filter isOccupied . findNeighbours cells

isOccupied :: (Location, Cell) -> Bool
isOccupied (_, Seat Occupied) = True
isOccupied _ = False

findNeighbours :: Cells -> Location -> Cells
findNeighbours cells location =
  filter (isNeighbour location) cells

isNeighbour :: Location -> (Location, Cell) -> Bool
isNeighbour location (location', _) =
  abs (row - row') <= 1
    && abs (column - column') <= 1
    && location /= location'
  where
    row = y location
    column = x location
    row' = y location'
    column' = x location'

-- Parse input

linesAsCells :: String -> Cells
linesAsCells =
  concatMap parseRow . zip [0 ..] . lines

parseRow :: (Int, String) -> Cells
parseRow (rowNumber, row) =
  zipWith (parseCell rowNumber) [0 ..] row

parseCell :: Int -> Int -> Char -> (Location, Cell)
parseCell rowNumber columnNumber char =
  case char of
    '.' -> (location, Floor)
    '#' -> (location, Seat Occupied)
    'L' -> (location, Seat Empty)
  where
    location = Location columnNumber rowNumber

-- Print

showCells :: Cells -> String
showCells [] = ""
showCells cells =
  (map toChar . take 10 $ cells) ++ "\n" ++ showCells (drop 10 cells)

toChar :: (Location, Cell) -> Char
toChar (_, Floor) = '.'
toChar (_, Seat Occupied) = '#'
toChar (_, Seat Empty) = 'L'
