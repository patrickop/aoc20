module Day1 where

import Common

parseIntsFile :: String -> IO [Int]
parseIntsFile name = do
  lineList <- parseFileLines name
  return (map (read :: String -> Int) lineList)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

duos :: [Int] -> [[Int]]
duos values = [[i, j] | i <- values, j <- values, i < j]

triples :: [Int] -> [[Int]]
triples values =
  [[i, j, k] | i <- values, j <- values, k <- values, i < j, j < k]

addUpTo :: Int -> [Int] -> Bool
addUpTo goal xs = (sum xs) == goal

findCombosThatAddTo :: [[Int]] -> Int -> Maybe [Int]
findCombosThatAddTo list goal = head' (filter (addUpTo goal) list)

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


