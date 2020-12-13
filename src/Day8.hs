module Day8 where

import Common
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

data Opcode
  = Nop
  | Acc
  | Jmp
  deriving (Eq, Show)

type Instruction = (Opcode, Int)

data State =
  State
    { pc :: Int
    , acc :: Int
    , program :: [Instruction]
    }
  deriving (Eq, Show)

modifyPc :: Int -> State -> State
modifyPc change (State pc acc program) = (State (pc + change) acc program)

modifyAcc :: Int -> State -> State
modifyAcc change (State pc acc program) = (State pc (acc + change) program)

getInstruction :: State -> Maybe Instruction
getInstruction (State pc _ program)
  | pc < length program = Just (program !! pc)
  | otherwise = Nothing

executeInstruction :: State -> Instruction -> State
executeInstruction state (Nop, _) = modifyPc 1 state
executeInstruction state (Acc, change) = modifyAcc change $ modifyPc 1 state
executeInstruction state (Jmp, change) = modifyPc change state

tick :: State -> Maybe State
tick state
  | Just instruction <- (getInstruction state) =
    Just $ executeInstruction state instruction
  | otherwise = Nothing

tickFor :: Int -> State -> Maybe State
tickFor 0 state = Just state
tickFor n state
  | Just state' <- (tick state) = tickFor (n - 1) state'
  | otherwise = Nothing

readSigned :: String -> Int
readSigned ('+':xs) = read xs
readSigned ('-':xs) = -(read xs)
readSigned x = read x

parseInstruction :: [String] -> Instruction
parseInstruction ["nop", x] = (Nop, (readSigned x))
parseInstruction ["jmp", x] = (Jmp, (readSigned x))
parseInstruction ["acc", x] = (Acc, (readSigned x))

parseProgram :: String -> IO [Instruction]
parseProgram filename = do
  lines <- parseFileLines filename
  return $ map parseInstruction $ map (splitOn " ") lines

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = (notElem x xs) && (allDifferent xs)

allFutureStates :: State -> [Maybe State]
allFutureStates state = map (\x -> tickFor x state) [0 ..]

terminated :: State -> Bool
terminated state = (pc state) >= (length (program state))

allDifferentPcs = allDifferent . map pc

looped = not . allDifferentPcs

replaceOpcode :: [Instruction] -> Int -> Opcode -> [Instruction]
replaceOpcode xs index opcode =
  let (start, end) = splitAt index $ xs
      (_, operand) = head end
   in start ++ ([(opcode, operand)] ++ (tail end))

swapInstruction :: [Instruction] -> Int -> Maybe [Instruction]
swapInstruction xs index
  | index >= length xs = Nothing
  | opcode == Nop = Just (replaceOpcode xs index Jmp)
  | opcode == Jmp = Just (replaceOpcode xs index Nop)
  | otherwise = Nothing
  where
    opcode = fst $ xs !! index

loopsOrTerminates :: [Maybe State] -> Bool
loopsOrTerminates maybeStates =
  let loops = looped $ catMaybes maybeStates
      terminates = elem Nothing maybeStates
   in loops || terminates

finalState :: [State] -> Maybe State
finalState [] = Nothing
finalState states
  | (pc $ lastState) >= (length $ program lastState) = Just lastState
  | otherwise = Nothing
  where
    lastState = last states

a :: String -> IO Int
a filename = do
  code <- parseProgram filename
  let state = State 0 0 code
  let states = catMaybes $ allFutureStates state
  let nonRepeatingStates = takeWhileList allDifferentPcs [] states
  return $ acc $ last nonRepeatingStates

b :: String -> IO Int
b filename = do
  code <- parseProgram filename
  let locs = length code
  let alternatives = catMaybes $ map (swapInstruction code) [0 .. locs]
  let initialStates = map (State 0 0) alternatives
  let allHistories = map allFutureStates initialStates
  let allHistoriesFinite =
        map
          (catMaybes . (takeWhileList (not . loopsOrTerminates) []))
          allHistories
  let finalStates = catMaybes $ map finalState allHistoriesFinite
  return $ acc $ head finalStates
