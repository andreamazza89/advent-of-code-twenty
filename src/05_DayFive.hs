module DayFive where

import Data.List

data Ticket = Ticket
  { row :: Int,
    column :: Int,
    ticketId :: Int
  }
  deriving (Show)

instance Eq Ticket where
  ticket == ticket' = row ticket == row ticket'

instance Ord Ticket where
  ticket <= ticket' = row ticket <= row ticket'

solutions :: IO ()
solutions =
  readTickets "./data/05.data"
    >>= putStrLn . solveTwo

solveOne :: [Ticket] -> Int
solveOne =
  maximum . map ticketId

-- for this one I just print out the row with 7 seats and then eyeball the missing column
solveTwo :: [Ticket] -> String
solveTwo =
  intercalate "\n"
    . map show
    . head
    . filter ((== 7) . length)
    . Data.List.group
    . Data.List.sort

readTickets :: String -> IO [Ticket]
readTickets filePath =
  map parseTicket . lines <$> readFile filePath

parseTicket :: String -> Ticket
parseTicket rawTicket =
  Ticket row column id
  where
    row = findRow (take 7 rawTicket) [0 .. 127]
    column = findColumn (drop 7 rawTicket) [0 .. 7]
    id = row * 8 + column

findRow :: String -> [Int] -> Int
findRow _ [lastRow] = lastRow
findRow (instruction : otherInstructions) rowsLeft = findRow otherInstructions (slice instruction rowsLeft)

findColumn :: String -> [Int] -> Int
findColumn _ [lastColumn] = lastColumn
findColumn (instruction : otherInstructions) columnsLeft = findColumn otherInstructions (slice instruction columnsLeft)

slice :: Char -> [Int] -> [Int]
slice 'B' rows = drop (length rows `div` 2) rows
slice 'R' rows = drop (length rows `div` 2) rows
slice 'F' rows = take (length rows `div` 2) rows
slice 'L' rows = take (length rows `div` 2) rows
