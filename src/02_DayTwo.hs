module DayTwo (solutions) where

import Data.Functor.Identity
import Text.Parsec

data PasswordEntry = PasswordEntry
  { policy :: Policy,
    password :: String
  }
  deriving (Show)

data Policy = Policy
  { firstNumber :: Int,
    secondNumber :: Int,
    letter_ :: Char
  }
  deriving (Show)

solutions :: IO ()
solutions =
  linesAsPasswordEntries "./data/02.data"
    >>= ( \case
            Left _ -> print "parser failed"
            Right entries ->
              print (solveOne entries) >> print (solveTwo entries)
        )

solveOne :: [PasswordEntry] -> Int
solveOne = length . filter isValidPasswordOne

solveTwo :: [PasswordEntry] -> Int
solveTwo = length . filter isValidPasswordTwo

isValidPasswordOne :: PasswordEntry -> Bool
isValidPasswordOne PasswordEntry {policy, password} =
  characterOccurrences >= firstNumber policy
    && characterOccurrences <= secondNumber policy
  where
    characterOccurrences = countCharacter (letter_ policy) password

countCharacter :: Char -> String -> Int
countCharacter character =
  length . filter (== character)

isValidPasswordTwo :: PasswordEntry -> Bool
isValidPasswordTwo PasswordEntry {policy, password} =
  (firstCharacter == char && secondCharacter /= char)
    || (firstCharacter /= char && secondCharacter == char)
  where
    firstCharacter = password !! (zeroOffset . firstNumber $ policy)
    secondCharacter = password !! (zeroOffset . secondNumber $ policy)
    char = letter_ policy

zeroOffset :: Int -> Int
zeroOffset x = x - 1

linesAsPasswordEntries :: String -> IO (Either ParseError [PasswordEntry])
linesAsPasswordEntries filepath =
  Text.Parsec.parse parsePasswords "" <$> readFile filepath

parsePasswords :: ParsecT String () Identity [PasswordEntry]
parsePasswords =
  Text.Parsec.many1 (parseEntry <* Text.Parsec.char '\n')

parseEntry :: ParsecT String () Identity PasswordEntry
parseEntry =
  PasswordEntry <$> parsePolicy <*> parsePassword

parsePolicy :: ParsecT String () Identity Policy
parsePolicy =
  Policy
    <$> number
    <* Text.Parsec.char '-'
    <*> number
    <* Text.Parsec.char ' '
    <*> Text.Parsec.anyChar

number :: ParsecT String () Identity Int
number =
  read <$> Text.Parsec.many1 Text.Parsec.digit

parsePassword :: ParsecT String () Identity String
parsePassword =
  Text.Parsec.char ':'
    *> Text.Parsec.char ' '
    *> Text.Parsec.many1 Text.Parsec.letter
