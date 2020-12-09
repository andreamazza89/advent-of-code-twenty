module DayEight where

newtype Cursor
  = Cursor Int

newtype Accumulator
  = Accumulator Int

data Instruction
  = NoOp Int
  | Acc Int
  | Jump Int
  deriving (Eq, Show)

solutions :: IO ()
solutions =
  linesAsInstructions "./data/08.data"
    >>= print . solveTwo

solveOne :: [Instruction] -> (Bool, Int)
solveOne =
  executeInstructions

solveTwo :: [Instruction] -> Int
solveTwo =
  snd
    . head
    . filter fst
    . map executeInstructions
    . generateFlippedInstructions

generateFlippedInstructions :: [Instruction] -> [[Instruction]]
generateFlippedInstructions allInstructions =
  zipWith flipOp [0 ..]
    . replicate (length allInstructions)
    $ allInstructions

flipOp :: Int -> [Instruction] -> [Instruction]
flipOp index =
  updateAt index update
  where
    update instruction =
      case instruction of
        NoOp n -> Jump n
        Jump n -> NoOp n
        accumulator -> accumulator

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt index update xs =
  take index xs ++ [updated] ++ drop (index + 1) xs
  where
    updated = update $ xs !! index

executeInstructions :: [Instruction] -> (Bool, Int)
executeInstructions =
  executeInstructions_ (Cursor 0) (Accumulator 0) []

executeInstructions_ :: Cursor -> Accumulator -> [(Int, Instruction)] -> [Instruction] -> (Bool, Int)
executeInstructions_ cursor@(Cursor c) accumulator@(Accumulator a) alreadyExecuted allInstructions
  | (c, currentInstruction) `elem` alreadyExecuted = (False, a)
  | c + 1 > length allInstructions = (True, a)
  | otherwise =
    executeInstructions_
      newCursor
      newAccumulator
      newAlreadyExecuted
      allInstructions
  where
    currentInstruction = allInstructions !! c
    (newCursor, newAccumulator) = executeInstruction currentInstruction cursor accumulator
    newAlreadyExecuted = (c, currentInstruction) : alreadyExecuted

executeInstruction :: Instruction -> Cursor -> Accumulator -> (Cursor, Accumulator)
executeInstruction (NoOp _) (Cursor cursor) accumulator =
  (Cursor $ cursor + 1, accumulator)
executeInstruction (Acc delta) (Cursor cursor) (Accumulator accumulator) =
  (Cursor $ cursor + 1, Accumulator $ accumulator + delta)
executeInstruction (Jump jump) (Cursor cursor) accumulator =
  (Cursor $ cursor + jump, accumulator)

-- Parse instructions

linesAsInstructions :: String -> IO [Instruction]
linesAsInstructions filePath =
  map parseInstruction . lines <$> readFile filePath

parseInstruction :: String -> Instruction
parseInstruction input
  | take 3 input == "acc" = Acc number
  | take 3 input == "jmp" = Jump number
  | take 3 input == "nop" = NoOp number
  where
    absolute = read . drop 5 $ input
    number =
      if input !! 4 == '-'
        then - absolute
        else absolute
