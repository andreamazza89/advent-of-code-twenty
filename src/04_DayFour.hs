module DayFour where

import Data.Functor.Identity
import Data.List
import qualified Text.Parsec as Parser
import Text.Regex.TDFA

data Passport = Passport
  { birth :: String,
    country :: Maybe String,
    eyeColour :: String,
    expiration :: String,
    hairColour :: String,
    height :: String,
    issueYear :: String,
    pid :: String
  }
  deriving (Show)

solutions :: IO ()
solutions =
  linesAsPassports "./data/04.data"
    >>= print . solveTwo

solveOne :: [Either Parser.ParseError Passport] -> Int
solveOne =
  length . rights

solveTwo :: [Either Parser.ParseError Passport] -> Int
solveTwo =
  length . filter passesValidations . rights

-- part 2 validations

passesValidations :: Passport -> Bool
passesValidations passport =
  all
    ($ passport)
    [ checkBirthYear,
      checkIssueYear,
      checkExpirationYear,
      checkHeight,
      checkHairColour,
      checkEyeColour,
      checkPassportId
    ]

checkBirthYear :: Passport -> Bool
checkBirthYear passport =
  birthYear >= 1920
    && birthYear <= 2002
  where
    birthYear = read $ birth passport

checkIssueYear :: Passport -> Bool
checkIssueYear passport =
  issue >= 2010
    && issue <= 2020
  where
    issue = read $ issueYear passport

checkExpirationYear :: Passport -> Bool
checkExpirationYear passport =
  exp >= 2020
    && exp <= 2030
  where
    exp = read $ expiration passport

checkHeight :: Passport -> Bool
checkHeight passport
  | hgt =~ "...cm" = checkCm (read . take 3 $ hgt)
  | hgt =~ "..in" = checkIn (read . take 2 $ hgt)
  | otherwise = False
  where
      hgt = height passport
      checkCm amount = amount >= 150 && amount <= 193
      checkIn amount = amount >= 59 && amount <= 76

checkHairColour :: Passport -> Bool
checkHairColour passport =
  colour =~ "^#[0-9a-f]{6}$"
  where
    colour = hairColour passport

checkEyeColour :: Passport -> Bool
checkEyeColour passport =
  colour =~ "amb|blu|brn|gry|grn|hzl|oth"
  where
    colour = eyeColour passport

checkPassportId :: Passport -> Bool
checkPassportId =
  (==) 9 . length . pid
  
-- parsing the input - because I couldn't figure out how to parse from the unordered input,
-- I first process the input to sort the various 'tokens' and then parse them, expecting passport fields in alphabetical
-- order

linesAsPassports :: String -> IO [Either Parser.ParseError Passport]
linesAsPassports filePath =
  map (parsePassport . unwords . sort . words) . lines . removeExtraReturns "" <$> readFile filePath

parsePassport :: String -> Either Parser.ParseError Passport
parsePassport =
  Parser.parse passportParser ""

passportParser :: Parser.ParsecT String () Identity Passport
passportParser =
  Passport
    <$> parseUnvalidatedField "byr"
    <*> Parser.optionMaybe (parseUnvalidatedField "cid")
    <*> parseUnvalidatedField "ecl"
    <*> parseUnvalidatedField "eyr"
    <*> parseUnvalidatedField "hcl"
    <*> parseUnvalidatedField "hgt"
    <*> parseUnvalidatedField "iyr"
    <*> parseUnvalidatedField "pid"

parseUnvalidatedField :: String -> Parser.ParsecT String () Identity String
parseUnvalidatedField fieldName =
  Parser.string fieldName
    *> Parser.char ':'
    *> Parser.many1 (Parser.char '#' Parser.<|> Parser.letter Parser.<|> Parser.digit)
    <* Parser.optionMaybe (Parser.char ' ')

removeExtraReturns :: String -> String -> String
removeExtraReturns output ('\n' : '\n' : rest) = removeExtraReturns ('\n' : output) rest
removeExtraReturns output ('\n' : rest) = removeExtraReturns (' ' : output) rest
removeExtraReturns output (char : rest) = removeExtraReturns (char : output) rest
removeExtraReturns output [] = reverse output

-- Either helpers

isRight :: Either a b -> Bool
isRight a =
  case a of
    Right _ -> True
    Left _ -> False

rights :: [Either a b] -> [b]
rights x = [a | Right a <- x]

