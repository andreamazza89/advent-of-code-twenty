module DaySeven where

import Data.Functor.Identity
import Data.List
import Debug.Trace
import qualified Text.Parsec as Parser

solutions :: IO ()
solutions =
  linesAsBags "./data/07.data"
    >>= print . solveTwo

linesAsBags :: String -> IO [Bag]
linesAsBags filepath =
  rights . map parseBag . lines <$> readFile filepath

data Bag = Bag
  { colour :: String,
    contents :: [(Int, Bag)]
  }

instance Eq Bag where
  (==) bag otherBag = colour bag == colour otherBag

instance Show Bag where
  show bag = "__" ++ colour bag ++ "__"

findParents :: Maybe Bag -> [Bag] -> [Bag]
findParents Nothing _ = []
findParents (Just bag) allBags =
  parentsOfThisBag ++ concatMap (\p -> findParents (Just p) allBags) parentsOfThisBag
  where
    parentsOfThisBag = filter (contains bag) allBags

findParentOf :: Bag -> [Bag] -> Maybe Bag
findParentOf myBag =
  Data.List.find (contains myBag)

contains :: Bag -> Bag -> Bool
contains containee container =
  containee `elem` map snd (contents container)

countChildren :: Bag -> [Bag] -> Int
countChildren bag allBags =
  totalChildrenInThisBag + pleaseFindAGoodNameForThis
  where
    childrenOfThisBag :: [(Int, Bag)]
    childrenOfThisBag = concatMap contents . filter (== bag) $ allBags
    totalChildrenInThisBag :: Int
    totalChildrenInThisBag = sum . map fst $ childrenOfThisBag
    pleaseFindAGoodNameForThis :: Int
    pleaseFindAGoodNameForThis = sum . map (\(n, child) -> n * countChildren child allBags) $ childrenOfThisBag

solveOne :: [Bag] -> Int
solveOne =
  length . unique . findParents (Just goldBag)

solveTwo :: [Bag] -> Int
solveTwo =
  countChildren goldBag

goldBag :: Bag
goldBag =
  Bag "shiny gold" []

-- Parser

parseBag :: String -> Either Parser.ParseError Bag
parseBag =
  Parser.parse bagParser ""

bagParser :: Parser.ParsecT String () Identity Bag
bagParser =
  Bag <$> parseColour <*> parseContents

internalBagParser :: Parser.ParsecT String () Identity Bag
internalBagParser =
  (\variation space colour -> Bag (variation ++ space ++ colour) [])
    <$> Parser.many1 Parser.letter
    <*> Parser.many (Parser.char ' ')
    <*> Parser.many1 Parser.letter
    <* Parser.string " bag"
    <* Parser.optionMaybe (Parser.char 's')

parseColour :: Parser.ParsecT String () Identity String
parseColour =
  (\variation space colour -> variation ++ space ++ colour)
    <$> Parser.many1 Parser.letter
    <*> Parser.many (Parser.char ' ')
    <*> Parser.many1 Parser.letter
    <* Parser.string " bag"
    <* Parser.optionMaybe (Parser.char 's')
    <* Parser.string " contain "

parseContents :: Parser.ParsecT String () Identity [(Int, Bag)]
parseContents =
  Parser.many1 ((,) <$> parseQuantity <*> internalBagParser <* Parser.anyChar <* Parser.optionMaybe (Parser.char ' '))
    Parser.<|> ([] <$ Parser.string "no other bags.")

parseQuantity :: Parser.ParsecT String () Identity Int
parseQuantity =
  read <$> Parser.many1 Parser.digit <* Parser.char ' '

rights :: [Either a b] -> [b]
rights x = [a | Right a <- x]

unique = Data.List.nub
