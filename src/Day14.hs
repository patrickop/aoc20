module Day14 where

import Common
import Data.Bits
import qualified Data.Map as M
import Data.Word
import Text.Regex.PCRE

data BitOperation
  = Set
  | Unset
  | Keep
  deriving (Eq, Show)

type Bitmask = [BitOperation]

data MemoryManipulation =
  MemoryManipulation
    { address :: Word
    , newValue :: Word
    }
  deriving (Eq, Show)

type Memory = M.Map Word Word

data State =
  State
    { bitmask :: Bitmask
    , memory :: Memory
    }
  deriving (Show)

data Instruction
  = BitmaskInstruction Bitmask
  | MemoryManipulationInstruction MemoryManipulation
  deriving (Eq, Show)

bitmaskRegex = "mask = ([X01]+)"

memoryRegex = "mem\\[([0-9]+)\\] = ([0-9]+)"

parseBitOperation :: Char -> BitOperation
parseBitOperation 'X' = Keep
parseBitOperation '1' = Set
parseBitOperation '0' = Unset

parseInstruction :: String -> Instruction
parseInstruction raw
  | raw =~ bitmaskRegex =
    BitmaskInstruction $ map parseBitOperation ((raw =~ bitmaskRegex) !! 0 !! 1)
  | raw =~ memoryRegex =
    let matches = raw =~ memoryRegex :: [[String]]
     in MemoryManipulationInstruction $
        MemoryManipulation (read $ matches !! 0 !! 1) (read $ matches !! 0 !! 2)
  | otherwise = error "bad parse"

applyBitOperation :: Word -> (Int, BitOperation) -> Word
applyBitOperation w (_, Keep) = w
applyBitOperation w (i, Set) = setBit w i
applyBitOperation w (i, Unset) = clearBit w i

applyBitmask :: Word -> Bitmask -> Word
applyBitmask v m = foldl applyBitOperation v (zip [0 ..] (reverse m))

applyBitOperationsAddress :: Word -> [(Int, BitOperation)] -> [Word]
applyBitOperationsAddress w ((i, Keep):xs) =
  (applyBitOperationsAddress (setBit w i) xs) ++
  (applyBitOperationsAddress (clearBit w i) xs)
applyBitOperationsAddress w ((i, Set):xs) =
  applyBitOperationsAddress (setBit w i) xs
applyBitOperationsAddress w ((i, Unset):xs) = applyBitOperationsAddress w xs
applyBitOperationsAddress w [] = [w]

applyBitmaskAddress :: Word -> Bitmask -> [Word]
applyBitmaskAddress v m = applyBitOperationsAddress v (zip [0 ..] (reverse m))

applyMemory :: Memory -> MemoryManipulation -> Bitmask -> Memory
applyMemory m (MemoryManipulation a v) bm = M.insert a (applyBitmask v bm) m

tick :: State -> Instruction -> State
tick (State b m) (BitmaskInstruction b1) = State b1 m
tick (State b m) (MemoryManipulationInstruction mm) =
  State b (applyMemory m mm b)

applyMemory2 :: Memory -> MemoryManipulation -> Bitmask -> Memory
applyMemory2 m (MemoryManipulation a v) bm =
  foldl (\mn an -> M.insert an v mn) m (applyBitmaskAddress a bm)

tick2 :: State -> Instruction -> State
tick2 (State b m) (BitmaskInstruction b1) = State b1 m
tick2 (State b m) (MemoryManipulationInstruction mm) =
  State b (applyMemory2 m mm b)

a :: String -> IO Int
a filename = do
  lines <- parseFileLines filename
  let instructions = map parseInstruction lines
  let finalState =
        foldl tick (State (take 36 $ repeat Keep) (M.fromList [])) instructions
  let result = M.foldr (+) 0 (memory finalState)
  return $ fromIntegral result

b :: String -> IO Int
b filename = do
  lines <- parseFileLines filename
  let instructions = map parseInstruction lines
  let finalState =
        foldl tick2 (State (take 36 $ repeat Keep) (M.fromList [])) instructions
  let result = M.foldr (+) 0 (memory finalState)
  return $ fromIntegral result
