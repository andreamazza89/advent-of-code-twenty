module DayEleven where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
  deriving (Eq, Ord, Show)

type Cells = Map Location Cell

solutions :: IO ()
solutions =
  readFile "./data/11.data"
    >>= print . solveOne . linesAsCells


solveOne :: Cells -> Int
solveOne =
  length . Map.filter isOccupied . solveOne_ Map.empty

solveOne_ :: Cells -> Cells -> Cells
solveOne_ cells cells'
  | cells == cells' = cells
  | otherwise =  solveOne_ cells' (tick cells')

tick :: Cells -> Cells
tick cells =
  Map.mapWithKey (evolveCell cells) cells

evolveCell :: Cells -> Location -> Cell -> Cell
evolveCell cells location (Seat Empty) =
  if occupiedNearby cells location == 0
    then Seat Occupied
    else Seat Empty
evolveCell cells location (Seat Occupied) =
  if occupiedNearby cells location >= 4
    then Seat Empty
    else Seat Occupied
evolveCell _ _ cell = cell

occupiedNearby :: Cells -> Location -> Int
occupiedNearby cells =
  length . Map.filter isOccupied . findNeighbours cells

isOccupied :: Cell -> Bool
isOccupied (Seat Occupied) = True
isOccupied _ = False

findNeighbours :: Cells -> Location -> Cells
findNeighbours cells location =
  Map.filterWithKey (isNeighbour location) cells

isNeighbour :: Location -> Location -> Cell -> Bool
isNeighbour location location' _ =
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
  Map.fromList . concatMap parseRow . zip [0 ..] . lines

parseRow :: (Int, String) -> [(Location, Cell)]
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

--showCells :: Cells -> String
--showCells [] = ""
--showCells cells =
--  (map toChar . take 10 $ cells) ++ "\n" ++ showCells (drop 10 cells)

toChar :: (Location, Cell) -> Char
toChar (_, Floor) = '.'
toChar (_, Seat Occupied) = '#'
toChar (_, Seat Empty) = 'L'
