module DayTwelve where

data Position = Position
  { x :: Int,
    y :: Int,
    orientation :: Orientation
  }
  deriving (Show)

data Orientation
  = North
  | East
  | South
  | West
  deriving (Show)

data Direction
  = Left_
  | Right_

type Degrees = Int

data Instruction
  = Move Orientation Steps
  | MoveForward Steps
  | Turn Direction Degrees

type Steps = Int

solutions :: IO ()
solutions =
  readFile "./data/12.data"
    >>= print . followWayPoint . linesAsInstructions

followWayPoint :: [Instruction] -> (Position, Position) --  ship is first, waypoint second
followWayPoint =
  foldl stepWithWaypoint (Position 0 0 East, Position 10 1 East)

stepWithWaypoint :: (Position, Position) -> Instruction -> (Position, Position)
stepWithWaypoint (shipPosition, wayPosition) (Turn direction degrees) =
  (shipPosition, rotateWayPoint wayPosition direction degrees)
stepWithWaypoint (shipPosition, wayPosition@Position {x, y}) (MoveForward steps) =
  (moveTowardsWayPoint shipPosition x y steps, wayPosition)
stepWithWaypoint (shipPosition, wayPosition) (Move orientation steps) =
  (shipPosition, move wayPosition orientation steps)

moveTowardsWayPoint :: Position -> Int -> Int -> Steps -> Position
moveTowardsWayPoint p@Position {x, y} x' y' steps =
  p {x = x + (x' * steps), y = y + (y' * steps)}

followInstructions :: [Instruction] -> Position
followInstructions =
  foldl (flip step) (Position 0 0 East)

step :: Instruction -> Position -> Position
step (Turn direction degrees) p@Position {orientation} =
  p {orientation = turn orientation direction degrees}
step (MoveForward steps) position =
  moveForward position steps
step (Move orientation steps) position =
  move position orientation steps

move :: Position -> Orientation -> Steps -> Position
move p@Position {y} North steps =
  p {y = y + steps}
move p@Position {x} East steps =
  p {x = x + steps}
move p@Position {y} South steps =
  p {y = y - steps}
move p@Position {x} West steps =
  p {x = x - steps}

moveForward :: Position -> Steps -> Position
moveForward p@Position {y, orientation = North} steps =
  p {y = y + steps}
moveForward p@Position {x, orientation = East} steps =
  p {x = x + steps}
moveForward p@Position {y, orientation = South} steps =
  p {y = y - steps}
moveForward p@Position {x, orientation = West} steps =
  p {x = x - steps}

turn :: Orientation -> Direction -> Degrees -> Orientation
turn North Left_ 90 = West
turn North Right_ 90 = East
turn North _ 180 = South
turn North Left_ 270 = East
turn North Right_ 270 = West
turn East Left_ 90 = North
turn East Right_ 90 = South
turn East _ 180 = West
turn East Left_ 270 = South
turn East Right_ 270 = North
turn South Left_ 90 = East
turn South Right_ 90 = West
turn South _ 180 = North
turn South Left_ 270 = West
turn South Right_ 270 = East
turn West Left_ 90 = South
turn West Right_ 90 = North
turn West _ 180 = East
turn West Left_ 270 = North
turn West Right_ 270 = South

rotateWayPoint :: Position -> Direction -> Degrees -> Position
rotateWayPoint p@Position {x, y} Left_ 90 = p {x = - y, y = x}
rotateWayPoint p@Position {x, y} _ 180 = p {x = - x, y = - y}
rotateWayPoint p@Position {x, y} Left_ 270 = p {x = y, y = - x}
rotateWayPoint p@Position {x, y} Right_ 90 = p {x = y, y = - x}
rotateWayPoint p@Position {x, y} Right_ 270 = p {x = - y, y = x}

linesAsInstructions :: String -> [Instruction]
linesAsInstructions =
  map toInstruction
    . lines

toInstruction :: String -> Instruction
toInstruction ('F' : steps) = MoveForward $ read steps
toInstruction ('N' : steps) = Move North $ read steps
toInstruction ('E' : steps) = Move East $ read steps
toInstruction ('S' : steps) = Move South $ read steps
toInstruction ('W' : steps) = Move West $ read steps
toInstruction ('R' : degrees) = Turn Right_ $ read degrees
toInstruction ('L' : degrees) = Turn Left_ $ read degrees
