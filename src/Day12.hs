module Day12 where

import Common

data Action
  = MoveNorth
  | MoveSouth
  | MoveEast
  | MoveWest
  | TurnLeft
  | TurnRight
  | MoveForward

data Direction
  = North
  | South
  | East
  | West

type Position = (Int, Int)

type Instruction = (Action, Int)

data State =
  State
    { dir :: Direction
    , pos :: Position
    }

data StateB =
  StateB
    { wp :: Position
    , posB :: Position
    }

initialState = State East (0, 0)

initialStateB = StateB (1, 10) (0, 0)

manhattanDistance (n, e) = (abs n) + (abs e)

parseAction :: Char -> Action
parseAction 'N' = MoveNorth
parseAction 'S' = MoveSouth
parseAction 'E' = MoveEast
parseAction 'W' = MoveWest
parseAction 'L' = TurnLeft
parseAction 'R' = TurnRight
parseAction 'F' = MoveForward

parseInstruction :: String -> Instruction
parseInstruction (x:xs) = (parseAction x, read xs)

turnRightBy90 :: Direction -> Direction
turnRightBy90 North = East
turnRightBy90 East = South
turnRightBy90 South = West
turnRightBy90 West = North

turnLeftBy90 :: Direction -> Direction
turnLeftBy90 North = West
turnLeftBy90 East = North
turnLeftBy90 South = East
turnLeftBy90 West = South

turnRight :: Direction -> Int -> Direction
turnRight d 0 = d
turnRight d x = turnRight (turnRightBy90 d) (x - 90)

turnLeft :: Direction -> Int -> Direction
turnLeft d 0 = d
turnLeft d x = turnLeft (turnLeftBy90 d) (x - 90)

executeInstruction :: State -> Instruction -> State
executeInstruction (State d (n, e)) (MoveNorth, o) = State d (n + o, e)
executeInstruction (State d (n, e)) (MoveEast, o) = State d (n, e + o)
executeInstruction (State d (n, e)) (MoveSouth, o) = State d (n - o, e)
executeInstruction (State d (n, e)) (MoveWest, o) = State d (n, e - o)
executeInstruction (State d pos) (TurnRight, o) = State (turnRight d o) pos
executeInstruction (State d pos) (TurnLeft, o) = State (turnLeft d o) pos
executeInstruction (State North (n, e)) (MoveForward, o) =
  State North (n + o, e)
executeInstruction (State South (n, e)) (MoveForward, o) =
  State South (n - o, e)
executeInstruction (State East (n, e)) (MoveForward, o) = State East (n, e + o)
executeInstruction (State West (n, e)) (MoveForward, o) = State West (n, e - o)

turnWpRightBy90 :: Position -> Position
turnWpRightBy90 (n, e) = ((-e), n)

turnWpLeftBy90 :: Position -> Position
turnWpLeftBy90 (n, e) = (e, (-n))

turnWpRight :: Position -> Int -> Position
turnWpRight p 0 = p
turnWpRight p x = turnWpRight (turnWpRightBy90 p) (x - 90)

turnWpLeft :: Position -> Int -> Position
turnWpLeft p 0 = p
turnWpLeft p x = turnWpLeft (turnWpLeftBy90 p) (x - 90)

executeInstructionB :: StateB -> Instruction -> StateB
executeInstructionB (StateB (wpn, wpe) p) (MoveNorth, o) =
  StateB (wpn + o, wpe) p
executeInstructionB (StateB (wpn, wpe) p) (MoveSouth, o) =
  StateB (wpn - o, wpe) p
executeInstructionB (StateB (wpn, wpe) p) (MoveEast, o) =
  StateB (wpn, wpe + o) p
executeInstructionB (StateB (wpn, wpe) p) (MoveWest, o) =
  StateB (wpn, wpe - o) p
executeInstructionB (StateB (wpn, wpe) (n, e)) (MoveForward, o) =
  StateB (wpn, wpe) (n + (wpn * o), e + (wpe * o))
executeInstructionB (StateB w p) (TurnRight, o) = StateB (turnWpRight w o) p
executeInstructionB (StateB w p) (TurnLeft, o) = StateB (turnWpLeft w o) p

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let instructions = map parseInstruction lines
  let finalState = foldl executeInstruction initialState instructions
  return $ manhattanDistance $ pos finalState

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  let instructions = map parseInstruction lines
  let finalState = foldl executeInstructionB initialStateB instructions
  return $ manhattanDistance $ posB finalState
