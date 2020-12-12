module Day1 where

import Common

triples :: [Int] -> [[Int]]
triples values =
  [[i, j, k] | i <- values, j <- values, k <- values, i < j, j < k]

multiply' :: Maybe [Int] -> Int
multiply' (Just xs) = product xs

a :: String -> IO Int
a filename = do
  numbers <- parseIntsFile filename
  let t = duos numbers
  let combination = findCombosThatAddTo t 2020
  let result = multiply' combination
  return result

b :: String -> IO Int
b filename = do
  numbers <- parseIntsFile filename
  let t = triples numbers
  let combination = findCombosThatAddTo t 2020
  let result = multiply' combination
  return result
