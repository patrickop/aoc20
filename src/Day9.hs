module Day9 where

import Common
import Data.Maybe

getNumberWithPreamble :: Int -> [Int] -> Int -> Maybe ([Int], Int)
getNumberWithPreamble preambleLength xs index
  | index > 0
  , index < (length xs)
  , index > preambleLength =
    let (start, (x:_)) = splitAt index xs
        preamble = drop (index - preambleLength) start
     in Just (preamble, x)
  | otherwise = Nothing

isValid :: ([Int], Int) -> Bool
isValid (preamble, number) =
  not $ isNothing $ findCombosThatAddTo (duos preamble) number

findFirstInvalidNumber :: Int -> [Int] -> Int
findFirstInvalidNumber preambleLength xs =
  let len = length xs
      withPreamble =
        catMaybes $
        map (getNumberWithPreamble preambleLength xs) [preambleLength .. len]
      invalidNumbers = filter (not . isValid) withPreamble
   in snd $ head invalidNumbers

findContiguousRangeThatAddsTo :: Int -> [Int] -> [Int] -> [Int]
findContiguousRangeThatAddsTo goal targetRange sourceRange
  | goal == (sum targetRange) = targetRange
  | goal < (sum targetRange) =
    findContiguousRangeThatAddsTo goal (tail targetRange) sourceRange
  | goal > (sum targetRange) =
    findContiguousRangeThatAddsTo
      goal
      (targetRange ++ [head sourceRange])
      (tail sourceRange)

a' :: Int -> String -> IO Int
a' preambleLength filename = do
  lines <- parseIntsFile filename
  return $ findFirstInvalidNumber preambleLength lines

a = a' 25

b' :: Int -> String -> IO Int
b' preambleLength filename = do
  lines <- parseIntsFile filename
  let invalidNumber = findFirstInvalidNumber preambleLength lines
  let range = findContiguousRangeThatAddsTo invalidNumber [] lines
  return ((minimum range) + (maximum range))

b = b' 25
