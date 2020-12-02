module DayTwo (solutions) where

data PasswordEntry = PasswordEntry
  { policy :: Policy,
    password :: String
  }
  deriving (Show)

data Policy = Policy
  { firstNumber :: Int,
    secondNumber :: Int,
    letter :: Char
  }
  deriving (Show)

solutions :: IO ()
solutions =
  linesAsPasswordEntries "./data/02.data"
    >>= print . solveTwo

solveOne :: [PasswordEntry] -> Int
solveOne = length . filter isValidPasswordOne

solveTwo :: [PasswordEntry] -> Int
solveTwo = length . filter isValidPasswordTwo

isValidPasswordOne :: PasswordEntry -> Bool
isValidPasswordOne entry =
  characterOccurrences >= (firstNumber . policy $ entry)
    && characterOccurrences <= (secondNumber . policy $ entry)
  where
    characterOccurrences = countCharacter (letter . policy $ entry) (password entry)

countCharacter :: Char -> String -> Int
countCharacter character =
  length . filter (== character)

isValidPasswordTwo :: PasswordEntry -> Bool
isValidPasswordTwo entry =
  (firstCharacter == (letter . policy $ entry) && secondCharacter /= (letter . policy $ entry))
    || (firstCharacter /= (letter . policy $ entry) && secondCharacter == (letter . policy $ entry))
  where
    firstCharacter = password entry !! (zeroOffset . firstNumber . policy $ entry)
    secondCharacter = password entry !! (zeroOffset . secondNumber . policy $ entry)

zeroOffset :: Int -> Int
zeroOffset x = x - 1

-- Parsing the input as password entries; would be good to use a parser instead

linesAsPasswordEntries :: String -> IO [PasswordEntry]
linesAsPasswordEntries filepath =
  map parseEntry . lines <$> readFile filepath

parseEntry :: String -> PasswordEntry
parseEntry line =
  PasswordEntry
    (parsePolicy line)
    (tail . dropWhile notASpace . tail . dropWhile notASpace $ line)

parsePolicy :: String -> Policy
parsePolicy line =
  Policy
    (read . takeWhile notADash $ line)
    (read . takeWhile notASpace . tail . dropWhile notADash $ line)
    ((!! 1) . dropWhile notASpace $ line)

notASpace :: Char -> Bool
notASpace = (/=) ' '

notADash :: Char -> Bool
notADash = (/=) '-'
